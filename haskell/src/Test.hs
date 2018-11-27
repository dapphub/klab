{-# LANGUAGE DeriveGeneric #-}
module Test where

import GHC.Generics
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson                (FromJSON, decode)
import Data.Either               (lefts, rights)
import Data.List                 (intercalate)

import Gas

data Kast = Kast {
  node     :: String,
  sort     :: Maybe String,
  name     :: Maybe String,
  token    :: Maybe String,
  label    :: Maybe String,
  variable :: Maybe Bool,
  arity    :: Maybe Int,
  args     :: Maybe [Kast]
  } deriving (Generic, Show)

instance FromJSON Kast
