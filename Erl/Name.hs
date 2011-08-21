module Erl.Name where

import qualified Data.Text as T

newtype Name = Name T.Text deriving (Eq, Ord, Show)
