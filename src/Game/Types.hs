{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Types where
import qualified Data.Matrix as MTX
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
  board :: MTX.Matrix (MTX.Matrix Square),
  metaBoard :: MTX.Matrix Square,
  moves :: Int
} deriving (Generic, Show)

--newState playerX playerO = Game playerX playerO (Move "None" 0 0) MTX.fromList 3 3 (map (MTX.fromList 3 3 (map (const Empty) [1..9])) [1..9]) MTX.fromList 3 3 (map (const Empty) [1..9]) 0

emptyState pX pO = Game {
        playerX = pX,
        playerO = pO,
        lastMove = (Move "None" 0 0),
--        board = MTX.fromList 3 3 $ map (
--            const (MTX.fromList 3 3 (map (const Empty) [1..9]))
--        ) [1..9],
--        metaBoard = MTX.fromList 3 3 (map (const Empty) [1..9]),
		board = MTX.fromList 3 3 $ map (const emptyBoard) [1..9],
		metaBoard = emptyBoard,
        moves = 0
    }
    where
    	emptyBoard = MTX.fromList 3 3 (map (const Empty) [1..9])


$(deriveJSON defaultOptions ''MTX.Matrix)  --Thank god for template haskell

instance ToJSON Move
instance FromJSON Move
instance ToJSON Square
instance FromJSON Square
instance ToJSON Game
instance FromJSON Game
