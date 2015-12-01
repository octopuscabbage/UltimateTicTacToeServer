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
  board :: Matrix (Matrix Square),
  metaBoard :: Matrix Square,
  moves :: Int
} deriving (Generic, Show)

newGame:: String -> String -> Game
newGame playerX playerO = Game playerX playerO (Move "None" 0 0) (fromList 3 3 (map (\_ -> fromList 3 3 (map (const Empty) [1..9])) [1..9])) (fromList 3 3 (map (const Empty) [1..9])) 0


$(deriveJSON defaultOptions ''Matrix)  --Thank god for template haskell

instance ToJSON Move
instance FromJSON Move
instance ToJSON Square
instance FromJSON Square
instance ToJSON Game
instance FromJSON Game
