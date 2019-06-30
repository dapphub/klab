{-# LANGUAGE DeriveGeneric #-}
module Kast where

import GHC.Generics
import Data.Aeson (FromJSON)

data Kast = Kast {
  node         :: String,
  sort         :: Maybe String,
  originalName :: Maybe String,
  token        :: Maybe String,
  label        :: Maybe String,
  variable     :: Maybe Bool,
  arity        :: Maybe Int,
  args         :: Maybe [Kast]
  } deriving (Generic, Eq, Ord, Show)

data Kasts = Kasts [Kast]
  deriving (Generic, Eq, Show)

instance FromJSON Kast
instance FromJSON Kasts

-- raiseNothing :: MonadError e m => e -> Maybe a -> m a
-- raiseNothing err x = case x of
--   Just x -> return x
--   Nothing -> throwError err

-- peek :: (MonadError e m)
--   => (r -> Maybe a) -> r -> m a
-- peek getter x =
