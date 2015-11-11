{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where
import Data.Matrix
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
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



$(deriveJSON defaultOptions ''Matrix)  --Thank god for template haskell

instance ToJSON Move
instance FromJSON Move
instance ToJSON Square
instance FromJSON Square
instance ToJSON Game
instance FromJSON Game
