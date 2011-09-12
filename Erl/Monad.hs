{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Erl.Monad (ErlError(..),
                  MonadErl(..),
                  ErlT(..),
                  ErlState,
                  emptyState,
                  doErlT,
                  evalErlT,
                  execErlT,
                  Erl(..),
                  doErl,
                  evalErl,
                  execErl,
                  getEntitySet,
                  getEntityAttributes,
                  EntitySetId,
                  entitySetId,
                  BinRelId,
                  binRelId) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as DM
import qualified Data.Set as DS

import qualified Erl.Entity as E
import qualified Erl.EntitySet as ES
import qualified Erl.EntityMap as EM

data ErlError = ErlError String deriving (Eq, Ord, Show)

instance Error ErlError where
  strMsg = ErlError

class (MonadError ErlError m) => MonadErl d m | m -> d where
  createEntitySet :: m EntitySetId
  deleteEntitySet :: EntitySetId -> m ()
  lookupEntitySet :: EntitySetId -> m (Maybe ES.EntitySet)
  entitySetIds :: m [EntitySetId]
  createEntity :: m E.EntityId
  deleteEntity :: E.EntityId -> m ()
  hasEntity    :: E.EntityId -> m Bool
  addEntity    :: E.EntityId -> d -> EntitySetId -> m ()
  removeEntity :: E.EntityId -> EntitySetId -> m ()
  lookupEntityAttributes :: E.EntityId -> EntitySetId -> m (Maybe d)
  selectEntities :: (d -> Bool) -> m ES.EntitySet
  updateEntity :: E.EntityId -> (d -> d) -> m ()

getEntitySet :: (MonadErl d m) => EntitySetId -> m ES.EntitySet
getEntitySet esid = maybe noSuchEntitySet return =<< lookupEntitySet esid
  where noSuchEntitySet = throwMsg $ "No entity set for ID " ++ show esid ++ "."

getEntityAttributes :: (MonadErl d m) => E.EntityId -> EntitySetId -> m d
getEntityAttributes eid esid = maybe noSuchEntity return =<< lookupEntityAttributes eid esid
  where noSuchEntity = throwMsg $ "No entity with ID " ++ show eid ++ "."

newtype ErlT d m a =
  ErlT { runErlT :: ErrorT ErlError (StateT (ErlState d) m) a }
  deriving (Monad, MonadError ErlError, MonadState (ErlState d), MonadIO)

doErlT :: (Monad m) => ErlT d m a -> ErlState d -> m (Either ErlError a, ErlState d)
doErlT erl state = runStateT (runErrorT $ runErlT erl) state

evalErlT :: (Monad m) => ErlT d m a -> ErlState d -> m (Either ErlError a)
evalErlT erl state = evalStateT (runErrorT $ runErlT erl) state

execErlT :: (Monad m) => ErlT d m a -> ErlState d -> m (ErlState d)
execErlT erl state = execStateT (runErrorT $ runErlT erl) state

newtype Erl d a =
  Erl { runErl ::  ErlT d Identity a }
  deriving (Monad, MonadError ErlError, MonadErl d)

doErl :: Erl d a -> ErlState d -> (Either ErlError a, ErlState d)
doErl erl state = runIdentity $ doErlT (runErl erl) state

evalErl :: Erl d a -> ErlState d -> Either ErlError a
evalErl erl state = runIdentity $ evalErlT (runErl erl) state

execErl :: Erl d a -> ErlState d -> ErlState d
execErl erl state = runIdentity $ execErlT (runErl erl) state

instance (Monad m) => MonadErl d (ErlT d m) where
  createEntitySet = modify' esCreateEntitySet
  deleteEntitySet esid = modify (esDeleteEntitySet esid)
  lookupEntitySet esid = esLookupEntitySet esid `liftM` get
  entitySetIds = esEntitySetIds `liftM` get
  createEntity = modify' esCreateEntity
  deleteEntity eid = modify (esDeleteEntity eid)
  hasEntity eid = esHasEntity eid `liftM` get
  addEntity eid attrs esid = modify''' (esAddEntity eid attrs esid)
  removeEntity eid esid = modify''' (esRemoveEntity eid esid)
  lookupEntityAttributes eid esid = esLookupEntityAttributes eid esid `liftM` get
  selectEntities pred =  esSelectEntities pred `liftM` get
  updateEntity = ni

esCreateEntitySet :: ErlState d -> (EntitySetId, ErlState d)
esCreateEntitySet s = (r, s')
  where r = nextEntitySetId s
        s' = s { nextEntitySetId = succ r,
                 entitySets = DM.insert r rec (entitySets s) }
        rec = EntitySetRec { entitySet = ES.empty,
                             entityAttributes = EM.empty }

esDeleteEntitySet :: EntitySetId -> ErlState d -> ErlState d
esDeleteEntitySet esid s = s { entitySets = DM.delete esid (entitySets s) }

esLookupEntitySet :: EntitySetId -> ErlState d -> Maybe ES.EntitySet
esLookupEntitySet esid s = fmap entitySet $ DM.lookup esid (entitySets s)

esEntitySetIds :: ErlState d -> [EntitySetId]
esEntitySetIds = DM.keys . entitySets

esCreateEntity :: ErlState d -> (E.EntityId, ErlState d)
esCreateEntity s = (eid, s')
  where eid = nextEntityId s
        erec = EntityRec { inSets = DS.empty }
        s' = s { nextEntityId = succ eid,
                 entities = EM.insert eid erec (entities s) }

esHasEntity :: E.EntityId -> ErlState d -> Bool
esHasEntity eid s = EM.member eid $ entities s

esDeleteEntity :: E.EntityId -> ErlState d -> ErlState d
esDeleteEntity eid s = s { entities = EM.delete eid (entities s) }

esAddEntity :: E.EntityId -> d -> EntitySetId -> ErlState d -> Either ErlError (ErlState d)
esAddEntity eid attrs esid s = do
  esrec <- esGetEntitySetRec esid s
  erec  <- esGetEntityRec eid s
  let esrec' = esrec { entityAttributes = EM.insert eid attrs (entityAttributes esrec) }
      erec'  = erec { inSets = DS.insert esid (inSets erec) }
  return $ s { entitySets = DM.insert esid esrec' (entitySets s),
               entities = EM.insert eid erec' (entities s) }

esRemoveEntity :: E.EntityId -> EntitySetId -> ErlState d -> Either ErlError (ErlState d)
esRemoveEntity eid esid s = do
  esrec <- esGetEntitySetRec esid s
  erec  <- esGetEntityRec eid s
  let esrec' = esrec { entityAttributes = EM.delete eid (entityAttributes esrec) }
      erec'  = erec { inSets = DS.delete esid (inSets erec) }
  return $ s { entitySets = DM.insert esid esrec' (entitySets s),
               entities = EM.insert eid erec' (entities s) }

esSelectEntities :: (d -> Bool) -> ErlState d -> ES.EntitySet
esSelectEntities pred = ni

esLookupEntityAttributes :: E.EntityId -> EntitySetId -> ErlState d -> Maybe d
esLookupEntityAttributes eid esid s =
  (DM.lookup esid $ entitySets s) >>= (EM.lookup eid . entityAttributes)

esGetEntitySetRec :: EntitySetId -> ErlState d -> Either ErlError (EntitySetRec d)
esGetEntitySetRec esid s =
  maybe noSuchSet return $ DM.lookup esid $ entitySets s
    where noSuchSet = throwMsg $ "No such entity set " ++ show esid ++ "."

esGetEntityRec :: E.EntityId -> ErlState d -> Either ErlError EntityRec
esGetEntityRec eid s =
  maybe noSuchEntity return $ EM.lookup eid $ entities s
    where noSuchEntity = throwMsg $ "No such entity " ++ show eid ++ "."

data ErlState d = ErlState {
  nextEntitySetId :: EntitySetId,
  entitySets :: DM.Map EntitySetId (EntitySetRec d),
  nextEntityId :: E.EntityId,
  entities :: EM.EntityMap EntityRec
  } deriving (Show)

data EntitySetRec d = EntitySetRec {
  entitySet :: ES.EntitySet,
  entityAttributes :: EM.EntityMap d
  } deriving (Show)

data EntityRec = EntityRec {
  inSets :: DS.Set EntitySetId
  } deriving (Show)

emptyState :: ErlState d
emptyState = ErlState {
  nextEntitySetId = EntitySetId 0,
  entitySets = DM.empty,
  nextEntityId = E.EntityId 0,
  entities = EM.empty
  }

newtype EntitySetId = EntitySetId Int deriving (Eq, Ord, Enum, Show)

entitySetId :: Int -> EntitySetId
entitySetId = EntitySetId

newtype BinRelId = BinRelId Int deriving (Eq, Ord, Enum, Show)

binRelId :: Int -> BinRelId
binRelId = BinRelId

modify' :: (MonadState s m) => (s -> (a,s)) -> m a
modify' f = do
  (r, s') <- f `liftM` get
  put s'
  return r

modify'' :: (MonadError e m, MonadState s m) => (s -> Either e (a,s)) -> m a
modify'' f = do
  errOrPair <- f `liftM` get
  (r, s') <- either throwError return $ errOrPair
  put s'
  return r

modify''' :: (MonadError e m, MonadState s m) => (s -> Either e s) -> m ()
modify''' f = modify'' (\s -> f s >>= return . ((),))

throwMsg :: (Error e, MonadError e m) => String -> m a
throwMsg = throwError . strMsg

ni = error "Not implemented"
