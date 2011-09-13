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
                  getEntity,
                  selectEntities,
                  EntitySetId,
                  entitySetId,
                  BinRelId,
                  binRelId) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as DM
import qualified Data.Set as DS

import Erl.Entity (EntityId(..))
import qualified Erl.EntitySet as ES
import qualified Erl.EntityMap as EM
import qualified Erl.BinRel as BR

data ErlError = NoSuchEntitySet EntitySetId |
                NoSuchEntity EntityId |
                NoSuchBinRel BinRelId |
                GeneralError String deriving (Eq, Ord, Show)

instance Error ErlError where
  strMsg = GeneralError

class (MonadError ErlError m) => MonadErl d m | m -> d where
  createEntitySet :: m EntitySetId
  deleteEntitySet :: EntitySetId -> m ()
  lookupEntitySet :: EntitySetId -> m (Maybe ES.EntitySet)
  entitySetIds :: m [EntitySetId]
  createEntity :: m EntityId
  deleteEntity :: EntityId -> m ()
  hasEntity    :: EntityId -> m Bool
  entityIds    :: m [EntityId]
  addEntity    :: EntityId -> d -> EntitySetId -> m ()
  removeEntity :: EntityId -> EntitySetId -> m ()
  lookupEntity :: EntityId -> EntitySetId -> m (Maybe d)
  createBinRel :: m BinRelId
  deleteBinRel :: BinRelId -> m ()
  lookupBinRel :: BinRelId -> m (Maybe BR.BinRel)
  binRelIds    :: m [BinRelId]
  addPair      :: EntityId -> EntityId -> d -> BinRelId -> m ()
  removePair   :: EntityId -> EntityId -> BinRelId -> m ()
  lookupPair   :: EntityId -> EntityId -> BinRelId -> m (Maybe d)

getEntitySet :: (MonadErl d m) => EntitySetId -> m ES.EntitySet
getEntitySet esid = maybe (noSuchSet esid) return =<< lookupEntitySet esid

getEntity :: (MonadErl d m) => EntityId -> EntitySetId -> m d
getEntity eid esid = maybe (noSuchEntity eid) return =<< lookupEntity eid esid

selectEntities :: (MonadErl d m) => (EntityId -> m Bool) -> ES.EntitySet -> m ES.EntitySet
selectEntities mpred eset =
  ES.fromList `liftM` filterM mpred (ES.toList eset)
  
updateEntity :: (MonadErl d m) => EntityId -> (d -> m d) -> EntitySetId -> m ()
updateEntity eid f esid = do
  getEntity eid esid >>= f >>= flip (addEntity eid) esid

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
  entityIds = esEntityIds `liftM` get
  addEntity eid attrs esid = modify''' (esAddEntity eid attrs esid)
  removeEntity eid esid = modify''' (esRemoveEntity eid esid)
  lookupEntity eid esid = esLookupEntity eid esid `liftM` get
  createBinRel = ni
  deleteBinRel bid = ni
  lookupBinRel bid = ni
  binRelIds = ni   :: m [BinRelId]
  addPair eid1 eid2 val bid = ni
  removePair eid1 eid2 bid = ni
  lookupPair eid1 eid2 bid = ni

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

esCreateEntity :: ErlState d -> (EntityId, ErlState d)
esCreateEntity s = (eid, s')
  where eid = nextEntityId s
        erec = EntityRec { inSets = DS.empty }
        s' = s { nextEntityId = succ eid,
                 entities = EM.insert eid erec (entities s) }

esDeleteEntity :: EntityId -> ErlState d -> ErlState d
esDeleteEntity eid s = s { entities = EM.delete eid (entities s) }

esHasEntity :: EntityId -> ErlState d -> Bool
esHasEntity eid s = EM.member eid $ entities s

esEntityIds :: ErlState d -> [EntityId]
esEntityIds = EM.ids . entities

esAddEntity :: EntityId -> d -> EntitySetId -> ErlState d -> Either ErlError (ErlState d)
esAddEntity eid attrs esid s = do
  esrec <- esGetEntitySetRec esid s
  erec  <- esGetEntityRec eid s
  let esrec' = esrec { entitySet = ES.insert eid (entitySet esrec),
                       entityAttributes = EM.insert eid attrs (entityAttributes esrec) }
      erec'  = erec { inSets = DS.insert esid (inSets erec) }
  return $ s { entitySets = DM.insert esid esrec' (entitySets s),
               entities = EM.insert eid erec' (entities s) }

esRemoveEntity :: EntityId -> EntitySetId -> ErlState d -> Either ErlError (ErlState d)
esRemoveEntity eid esid s = do
  esrec <- esGetEntitySetRec esid s
  erec  <- esGetEntityRec eid s
  let esrec' = esrec { entitySet = ES.delete eid (entitySet esrec),
                       entityAttributes = EM.delete eid (entityAttributes esrec) }
      erec'  = erec { inSets = DS.delete esid (inSets erec) }
  return $ s { entitySets = DM.insert esid esrec' (entitySets s),
               entities = EM.insert eid erec' (entities s) }

esLookupEntity :: EntityId -> EntitySetId -> ErlState d -> Maybe d
esLookupEntity eid esid s =
  (DM.lookup esid $ entitySets s) >>= (EM.lookup eid . entityAttributes)

esGetEntitySetRec :: EntitySetId -> ErlState d -> Either ErlError (EntitySetRec d)
esGetEntitySetRec esid s =
  maybe (noSuchSet esid) return $ DM.lookup esid $ entitySets s

esGetEntityRec :: EntityId -> ErlState d -> Either ErlError EntityRec
esGetEntityRec eid s =
  maybe (noSuchEntity eid) return $ EM.lookup eid $ entities s

noSuchSet :: (MonadError ErlError m) => EntitySetId -> m a
noSuchSet esid = throwError $ NoSuchEntitySet esid

noSuchEntity :: (MonadError ErlError m) => EntityId -> m a
noSuchEntity eid = throwError $ NoSuchEntity eid

noSuchBinRel :: (MonadError ErlError m) => BinRelId -> m a
noSuchBinRel bid = throwError $ NoSuchBinRel bid

data ErlState d = ErlState {
  nextEntitySetId :: EntitySetId,
  entitySets :: DM.Map EntitySetId (EntitySetRec d),
  nextEntityId :: EntityId,
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
  nextEntityId = EntityId 0,
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
