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
                  entity,
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

entitySet :: (MonadErl d m) => EntitySetId -> m ES.EntitySet
entitySet esid = maybe noSuchEntitySet return =<< lookupEntitySet esid
  where noSuchEntitySet = throwMsg $ "No entity set for ID " ++ show esid ++ "."

entity :: (MonadErl d m) => E.EntityId -> m (E.Entity d)
entity id = maybe noSuchInstance return =<< lookupEntity id
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
  createEntitySet = ni
  destroyEntitySet = ni
  lookupEntitySet = ni
  selectEntities pred = (ES.fromList . map E.id . DM.elems . DM.filter pred' . allEntities) `liftM` get
    where pred' = pred . E.attributes
  lookupEntity id = (DM.lookup id . allEntities) `liftM` get
  createEntity attrs = do
    s <- get
    let id = nextEntityId s
        e  = E.Entity { E.id = id, E.attributes = attrs }
        s' = s { nextEntityId = succ id,
                 allEntities = DM.insert id e (allEntities s) }
    put s'
    return id
  deleteEntity id = modify $ \s ->
    s { allEntities = DM.delete id (allEntities s) }
  updateEntity = ni

data ErlState d = ErlState {
  nextEntityId :: E.EntityId,
  allEntities :: DM.Map E.EntityId (E.Entity d)
  } deriving (Show)

emptyState :: ErlState d
emptyState = ErlState {
  nextEntityId = E.EntityId 0,
  allEntities = DM.empty
  }

newtype EntitySetId = EntitySetId Int deriving (Eq, Ord, Enum, Show)

newtype BinRelId = BinRelId Int deriving (Eq, Ord, Enum, Show)

throwMsg :: (Error e, MonadError e m) => String -> m a
throwMsg = throwError . strMsg

ni = error "Not implemented"

