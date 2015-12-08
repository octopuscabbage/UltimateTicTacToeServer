{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Types where
import Data.Matrix
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
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
  board :: [[Square]],
  metaBoard :: [Square],
  moves :: Int,
  gameWon :: Square
} deriving (Generic, Show, Eq)

newGame:: String -> String -> Game
newGame playerX playerO = Game playerX playerO (Move "None" 0 0) (map (\_ -> (map (const Empty) [1..9])) [1..9]) (map (const Empty) [1..9]) 0 Empty

instance ToJSON Move
instance FromJSON Move
instance ToJSON Square
instance FromJSON Square
instance ToJSON Game
instance FromJSON Game
