module Erl.Name where

import Data.String
import qualified Data.Text as T

newtype Name = Name T.Text deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . T.pack
