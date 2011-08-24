{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Erl.Monad where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import Erl.Name
import qualified Erl.EntityType as ET
import qualified Erl.Entity as E

data ErlError = ErlError String deriving (Eq, Ord, Show)

instance Error ErlError where
  strMsg = ErlError

class (MonadError ErlError m) => MonadErl d m where
  createEntityType :: Name -> m ET.Id
  entityTypeIds :: m [ET.Id]
  entityType :: ET.Id -> m ET.EntityType
  entityIds :: ET.Id -> m [E.Id]
  entity :: E.Id -> m (E.Instance d)
  createEntity :: ET.Id -> Name -> d -> m E.Id
  deleteEntity :: E.Id -> m ()
  updateEntity :: E.Id -> (d -> d) -> m ()
  renameEntity :: E.Id -> Name -> m ()
  
newtype ErlT d m a =
  ErlT { runErlT :: ErrorT ErlError (StateT ErlTState m) a }
  deriving (Monad, (MonadError ErlError), MonadIO)

data ErlTState = ErlTState

newtype ErlMonad d a =
  ErlMonad { runErl ::  ErlT d Identity a }
  deriving (Monad, MonadError ErlError)

instance (Monad m) => MonadErl d (ErlT d m) where
  createEntityType = ni
  entityTypeIds = ni
  entityType = ni
  entityIds = ni
  entity = ni
  createEntity = ni
  deleteEntity = ni
  updateEntity = ni
  renameEntity = ni

ni = error "Not implemented"
