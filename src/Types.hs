{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where
import Data.Matrix
import GHC.Generics
import Data.Aeson
import qualified Data.Vector as V
import Data.Functor

data Move = Move {
  player :: String,
  outerPos :: Int,
  innerPos :: Int
} deriving (Generic, Show, Read, Eq)

data Square = X | O | Empty | Both deriving (Generic, Show, Read, Eq)

data Game = Game {
  playerX :: String,
  playerO :: String,
  lastMove :: Move,
  board :: Matrix (Matrix Square),
  metaBoard :: Matrix Square,
  moves :: Int
} deriving (Generic, Show)

instance (ToJSON a) => ToJSON (Matrix a) where
   toJSON = Array . V.fromList . map toJSON . toList

instance (FromJSON a) => FromJSON (Matrix a) where
  parseJSON  (Array v) = (fromList 9 9 . map parseJSON . V.toList) <$> v

instance ToJSON Move
instance FromJSON Move
instance ToJSON Square
instance FromJSON Square
instance ToJSON Game
instance FromJSON Game
