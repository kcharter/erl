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
                  getEntity,
                  EntitySetId,
                  BinRelId) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as DM

import qualified Erl.Entity as E
import qualified Erl.EntitySet as ES

data ErlError = ErlError String deriving (Eq, Ord, Show)

instance Error ErlError where
  strMsg = ErlError

class (MonadError ErlError m) => MonadErl d m | m -> d where
  createEntitySet :: m EntitySetId
  destroyEntitySet :: EntitySetId -> m ()
  lookupEntitySet :: EntitySetId -> m (Maybe ES.EntitySet)
  selectEntities :: (d -> Bool) -> m ES.EntitySet
  lookupEntity :: E.EntityId -> m (Maybe (E.Entity d))
  createEntity :: d -> m E.EntityId
  deleteEntity :: E.EntityId -> m ()
  updateEntity :: E.EntityId -> (d -> d) -> m ()

getEntitySet :: (MonadErl d m) => EntitySetId -> m ES.EntitySet
getEntitySet esid = maybe noSuchEntitySet return =<< lookupEntitySet esid
  where noSuchEntitySet = throwMsg $ "No entity set for ID " ++ show esid ++ "."

getEntity :: (MonadErl d m) => E.EntityId -> m (E.Entity d)
getEntity id = maybe noSuchInstance return =<< lookupEntity id
  where noSuchInstance = throwMsg $ "No instance for entity ID " ++ show id ++ "."

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
  destroyEntitySet esid = modify (esDeleteEntitySet esid)
  lookupEntitySet esid = esLookupEntitySet esid `liftM` get
  selectEntities pred =  esSelectEntities pred `liftM` get
  lookupEntity eid = esLookupEntity eid `liftM` get
  createEntity attrs = modify' (esCreateEntity attrs)
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

esSelectEntities :: (d -> Bool) -> ErlState d -> ES.EntitySet
esSelectEntities pred =
  ES.fromList . map E.id . DM.elems . DM.filter pred' . allEntities
    where pred' = pred . E.attributes

esLookupEntity :: E.EntityId -> ErlState d -> Maybe (E.Entity d)
esLookupEntity eid = DM.lookup eid . allEntities

esCreateEntity :: d -> ErlState d -> (E.EntityId, ErlState d)
esCreateEntity attrs s = (eid, s')
  where eid = nextEntityId s
        e  = E.Entity { E.id = eid, E.attributes = attrs }
        s' = s { nextEntityId = succ eid,
                 allEntities = DM.insert eid e (allEntities s) }

esDeleteEntity :: E.EntityId -> ErlState d -> ErlState d
esDeleteEntity eid s = s { allEntities = DM.delete eid (allEntities s) }

data ErlState d = ErlState {
  nextEntitySetId :: EntitySetId,
  entitySets :: DM.Map EntitySetId EntitySetRec,
  nextEntityId :: E.EntityId,
  allEntities :: DM.Map E.EntityId (E.Entity d)
  } deriving (Show)

data EntitySetRec = EntitySetRec {
  entitySet :: ES.EntitySet
  } deriving (Show)

emptyState :: ErlState d
emptyState = ErlState {
  nextEntitySetId = EntitySetId 0,
  entitySets = DM.empty,
  nextEntityId = E.EntityId 0,
  allEntities = DM.empty
  }

newtype EntitySetId = EntitySetId Int deriving (Eq, Ord, Enum, Show)

newtype BinRelId = BinRelId Int deriving (Eq, Ord, Enum, Show)

modify' :: (MonadState s m) => (s -> (a,s)) -> m a
modify' f = do
  (r, s') <- f `liftM` get
  put s'
  return r

throwMsg :: (Error e, MonadError e m) => String -> m a
throwMsg = throwError . strMsg

ni = error "Not implemented"
