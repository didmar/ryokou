{-# LANGUAGE DeriveGeneric #-}

module Place (Place(..)) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data Place = Place {
  name :: String,
  rating :: Integer,
  lat :: Float,
  lng :: Float,
  url :: String,
  desc :: String
} deriving (Show, Generic)

instance ToJSON Place
