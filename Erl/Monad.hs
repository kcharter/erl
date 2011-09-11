{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

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
  selectEntities :: (d -> Bool) -> m ES.EntitySet
  lookupEntityAttributes :: E.EntityId -> m (Maybe d)
  createEntity :: m E.EntityId
  hasEntity    :: E.EntityId -> m Bool
  deleteEntity :: E.EntityId -> m ()
  updateEntity :: E.EntityId -> (d -> d) -> m ()

getEntitySet :: (MonadErl d m) => EntitySetId -> m ES.EntitySet
getEntitySet esid = maybe noSuchEntitySet return =<< lookupEntitySet esid
  where noSuchEntitySet = throwMsg $ "No entity set for ID " ++ show esid ++ "."

getEntityAttributes :: (MonadErl d m) => E.EntityId -> m d
getEntityAttributes eid = maybe noSuchEntity return =<< lookupEntityAttributes eid
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
  selectEntities pred =  esSelectEntities pred `liftM` get
  lookupEntityAttributes eid = esLookupEntityAttributes eid `liftM` get
  createEntity = modify' esCreateEntity
  hasEntity eid = esHasEntity eid `liftM` get
  deleteEntity eid = modify (esDeleteEntity eid)
  updateEntity = ni

esCreateEntitySet :: ErlState d -> (EntitySetId, ErlState d)
esCreateEntitySet s = (r, s')
  where r = nextEntitySetId s
        s' = s { nextEntitySetId = succ r,
                 entitySets = DM.insert r rec (entitySets s) }
        rec = EntitySetRec { entitySet = ES.empty }

esDeleteEntitySet :: EntitySetId -> ErlState d -> ErlState d
esDeleteEntitySet esid s = s { entitySets = DM.delete esid (entitySets s) }

esLookupEntitySet :: EntitySetId -> ErlState d -> Maybe ES.EntitySet
esLookupEntitySet esid s = fmap entitySet $ DM.lookup esid (entitySets s)

esEntitySetIds :: ErlState d -> [EntitySetId]
esEntitySetIds = DM.keys . entitySets

esSelectEntities :: (d -> Bool) -> ErlState d -> ES.EntitySet
esSelectEntities pred =
  ES.fromList . DM.keys . DM.filter pred . entityAttributes

esLookupEntityAttributes :: E.EntityId -> ErlState d -> Maybe d
esLookupEntityAttributes eid s = DM.lookup eid $ entityAttributes s

esCreateEntity :: ErlState d -> (E.EntityId, ErlState d)
esCreateEntity s = (eid, s')
  where eid = nextEntityId s
        s' = s { nextEntityId = succ eid,
                 entities = EM.insert eid EntityRec (entities s) }

esHasEntity :: E.EntityId -> ErlState d -> Bool
esHasEntity eid s = EM.member eid $ entities s

esDeleteEntity :: E.EntityId -> ErlState d -> ErlState d
esDeleteEntity eid s = s { entities = EM.delete eid (entities s),
                           entityAttributes = DM.delete eid (entityAttributes s) }

data ErlState d = ErlState {
  nextEntitySetId :: EntitySetId,
  entitySets :: DM.Map EntitySetId EntitySetRec,
  nextEntityId :: E.EntityId,
  entities :: EM.EntityMap EntityRec,
  entityAttributes :: DM.Map E.EntityId d
  } deriving (Show)

data EntitySetRec = EntitySetRec {
  entitySet :: ES.EntitySet
  } deriving (Show)

data EntityRec = EntityRec {
  } deriving (Show)

emptyState :: ErlState d
emptyState = ErlState {
  nextEntitySetId = EntitySetId 0,
  entitySets = DM.empty,
  nextEntityId = E.EntityId 0,
  entities = EM.empty,
  entityAttributes = DM.empty
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

throwMsg :: (Error e, MonadError e m) => String -> m a
throwMsg = throwError . strMsg

ni = error "Not implemented"
