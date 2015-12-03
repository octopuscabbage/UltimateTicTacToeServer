module Lib where

import Game.Types
import Data.Matrix
import qualified Data.Vector as V
import Control.Monad.Trans.Either


maybeToError err Nothing = left err
maybeToError _  (Just a) = right a

--Use with matrix rows, columns, and diagonal as a list to determine if a player has won
checkWin :: Square -> [V.Vector Square] -> Bool
checkWin s xs = any (== True) $ map (\v -> V.all (\x -> x == s || x == Both) v) xs

--Given a matrix of squares this will return which square (X, O, Neither, Both) has won
giveWinner :: Matrix Square -> Square
giveWinner m
    | both                                       = Both
    | checkWin X items                           = X
    | checkWin O items                           = O
    | otherwise                                  = Empty
    where 
        rows = map (\x -> Data.Matrix.getRow x m) [1,2,3]
        columns = map (\x -> Data.Matrix.getCol x m) [1,2,3]
        leftDiag = [(Data.Matrix.getDiag m)]
        rightDiag = [V.fromList [(Data.Matrix.getElem 3 1 m), (Data.Matrix.getElem 2 2 m), (Data.Matrix.getElem 1 3 m)]]
        items = rows ++ columns ++ leftDiag ++ rightDiag
        both = (checkWin X items) && (checkWin O items)

--Given a game board, return a meta board showing state of all sub-boards
checkSubBoards :: Matrix (Matrix Square) -> Matrix Square
checkSubBoards m = fmap giveWinner m

updateBoard :: Matrix (Matrix Square) -> Move -> Square -> Matrix (Matrix Square)
updateBoard m (Move _ outer inner) s = setElem newInner ((div outer 3), (mod outer 3)) m
    where 
        innerBoard = getElem (div outer 3) (mod outer 3) m
        newInner = setElem s ((div inner 3), (mod inner 3)) innerBoard      

getPlayer :: Game -> Move -> Square
getPlayer (Game x o _ _ _ _ _) (Move p _ _)
    | p == x = X
    | p == o = O
