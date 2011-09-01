{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Erl.Monad (ErlError(..),
                  MonadErl(..),
                  ErlT(..),
                  ErlTState,
                  emptyState,
                  doErlT,
                  evalErlT,
                  execErlT,
                  ErlMonad(..),
                  doErl,
                  evalErl,
                  execErl) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as DM

import qualified Erl.Entity as E

data ErlError = ErlError String deriving (Eq, Ord, Show)

instance Error ErlError where
  strMsg = ErlError

class (MonadError ErlError m) => MonadErl d m | m -> d where
  selectEntities :: (d -> Bool) -> m [E.EntityId]
  lookupEntity :: E.EntityId -> m (Maybe (E.Entity d))
  createEntity :: d -> m E.EntityId
  deleteEntity :: E.EntityId -> m ()
  updateEntity :: E.EntityId -> (d -> d) -> m ()

entity :: (MonadErl d m) => E.EntityId -> m (E.Entity d)
entity id = maybe noSuchInstance return =<< lookupEntity id
  where noSuchInstance = throwMsg $ "No instance for entity ID " ++ show id ++ "."

newtype ErlT d m a =
  ErlT { runErlT :: ErrorT ErlError (StateT (ErlTState d) m) a }
  deriving (Monad, MonadError ErlError, MonadState (ErlTState d), MonadIO)

doErlT :: (Monad m) => ErlT d m a -> ErlTState d -> m (Either ErlError a, ErlTState d)
doErlT erl state = runStateT (runErrorT $ runErlT erl) state

evalErlT :: (Monad m) => ErlT d m a -> ErlTState d -> m (Either ErlError a)
evalErlT erl state = evalStateT (runErrorT $ runErlT erl) state

execErlT :: (Monad m) => ErlT d m a -> ErlTState d -> m (ErlTState d)
execErlT erl state = execStateT (runErrorT $ runErlT erl) state

newtype ErlMonad d a =
  ErlMonad { runErl ::  ErlT d Identity a }
  deriving (Monad, MonadError ErlError, MonadErl d)

doErl :: ErlMonad d a -> ErlTState d -> (Either ErlError a, ErlTState d)
doErl erl state = runIdentity $ doErlT (runErl erl) state

evalErl :: ErlMonad d a -> ErlTState d -> Either ErlError a
evalErl erl state = runIdentity $ evalErlT (runErl erl) state

execErl :: ErlMonad d a -> ErlTState d -> ErlTState d
execErl erl state = runIdentity $ execErlT (runErl erl) state

instance (Monad m) => MonadErl d (ErlT d m) where
  selectEntities pred = (map E.id . DM.elems . DM.filter pred' . allEntities) `liftM` get
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

data ErlTState d = ErlTState {
  nextEntityId :: E.EntityId,
  allEntities :: DM.Map E.EntityId (E.Entity d)
  } deriving (Show)

emptyState :: ErlTState d
emptyState = ErlTState {
  nextEntityId = E.EntityId 0,
  allEntities = DM.empty
  }

throwMsg :: (Error e, MonadError e m) => String -> m a
throwMsg = throwError . strMsg

ni = error "Not implemented"

