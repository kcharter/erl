{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Erl.Monad (ErlError(..),
                  MonadErl(..),
                  hasEntityTypeNamed,
                  entityType,
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

import Erl.Name
import qualified Erl.EntityType as ET
import qualified Erl.Entity as E

data ErlError = ErlError String deriving (Eq, Ord, Show)

instance Error ErlError where
  strMsg = ErlError

class (MonadError ErlError m) => MonadErl d m | m -> d where
  createEntityType :: Name -> m ET.Id
  entityTypeIds :: m [ET.Id]
  lookupEntityType :: ET.Id -> m (Maybe ET.EntityType)
  lookupEntityTypeName :: Name -> m (Maybe ET.Id)
  entityIds :: ET.Id -> m [E.Id]
  entity :: E.Id -> m (E.Instance d)
  createEntity :: ET.Id -> d -> m E.Id
  deleteEntity :: E.Id -> m ()
  updateEntity :: E.Id -> (d -> d) -> m ()

hasEntityTypeNamed :: (MonadErl d m) => Name -> m Bool
hasEntityTypeNamed name = maybe False (const True) `liftM` lookupEntityTypeName name

entityType :: (MonadErl d m) => ET.Id -> m ET.EntityType
entityType id = maybe noSuchType return =<< lookupEntityType id
  where noSuchType = throwMsg $ "There is no entity type with id " ++ show id ++ "."

newtype ErlT d m a =
  ErlT { runErlT :: ErrorT ErlError (StateT ErlTState m) a }
  deriving (Monad, MonadError ErlError, MonadState ErlTState, MonadIO)

doErlT :: (Monad m) => ErlT d m a -> ErlTState -> m (Either ErlError a, ErlTState)
doErlT erl state = runStateT (runErrorT $ runErlT erl) state

evalErlT :: (Monad m) => ErlT d m a -> ErlTState -> m (Either ErlError a)
evalErlT erl state = evalStateT (runErrorT $ runErlT erl) state

execErlT :: (Monad m) => ErlT d m a -> ErlTState -> m ErlTState
execErlT erl state = execStateT (runErrorT $ runErlT erl) state

newtype ErlMonad d a =
  ErlMonad { runErl ::  ErlT d Identity a }
  deriving (Monad, MonadError ErlError, MonadErl d)

doErl :: ErlMonad d a -> ErlTState -> (Either ErlError a, ErlTState)
doErl erl state = runIdentity $ doErlT (runErl erl) state

evalErl :: ErlMonad d a -> ErlTState -> Either ErlError a
evalErl erl state = runIdentity $ evalErlT (runErl erl) state

execErl :: ErlMonad d a -> ErlTState -> ErlTState
execErl erl state = runIdentity $ execErlT (runErl erl) state

instance (Monad m) => MonadErl d (ErlT d m) where
  createEntityType name = do
    s <- get
    maybe (makeNew s) alreadyExists $ DM.lookup name (typeIdsByName s)
      where alreadyExists = const $ throwMsg $ "There is already an entity type " ++ show name ++ "."
            makeNew s = put s' >> return etId
              where etId = nextEntityTypeId s
                    s' = s { nextEntityTypeId = succ etId,
                             typesById = DM.insert etId et (typesById s),
                             typeIdsByName = DM.insert name etId (typeIdsByName s) }
                    et = ET.EntityType { ET.id = etId, ET.name = name }
  entityTypeIds = (DM.keys . typesById) `liftM` get
  lookupEntityTypeName name = (DM.lookup name . typeIdsByName) `liftM` get
  lookupEntityType id = (DM.lookup id . typesById) `liftM` get
  entityIds = ni
  entity = ni
  createEntity = ni
  deleteEntity = ni
  updateEntity = ni

data ErlTState = ErlTState {
  nextEntityTypeId :: ET.Id,
  typesById :: DM.Map ET.Id ET.EntityType,
  typeIdsByName :: DM.Map Name ET.Id
  }

emptyState :: ErlTState
emptyState = ErlTState {
  nextEntityTypeId = ET.Id 0,
  typesById = DM.empty,
  typeIdsByName = DM.empty
  }

throwMsg :: (Error e, MonadError e m) => String -> m a
throwMsg = throwError . strMsg

ni = error "Not implemented"

instance Show ErlTState where
  show s = "ErlTState { entityTypes = " ++ show (map ET.name $ DM.elems $ typesById s) ++ " }"
