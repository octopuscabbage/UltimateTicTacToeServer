module Lib where

import Game.Types
import Data.Matrix
import qualified Data.Vector as V
import Control.Monad.Trans.Either


maybeToError err Nothing = left err
maybeToError _  (Just a) = right a

--Use with matrix rows, columns, and diagonal as a list to determine if a player has won
checkWin :: Square -> [[Square]] -> Bool
checkWin s xs = any (== True) $ map (\v -> all (\x -> x == s || x == Both) v) xs

--Given a matrix of squares this will return which square (X, O, Neither, Both) has won
giveWinner :: [Square] -> Square
giveWinner xs
    | both                                       = Both
    | checkWin X items                           = X
    | checkWin O items                           = O
    | otherwise                                  = Empty
    where
        m = fromList 3 3 xs	
        rows = map (\x -> Data.Matrix.getRow x m) [1,2,3]
        columns = map (\x -> Data.Matrix.getCol x m) [1,2,3]
        leftDiag = [(Data.Matrix.getDiag m)]
        rightDiag = [V.fromList [(Data.Matrix.getElem 3 1 m), (Data.Matrix.getElem 2 2 m), (Data.Matrix.getElem 1 3 m)]]
        items = map V.toList (rows ++ columns ++ leftDiag ++ rightDiag)
        both = (checkWin X items) && (checkWin O items)

--Given a game board, return a meta board showing state of all sub-boards
checkSubBoards :: [[Square]] -> [Square]
checkSubBoards m = fmap giveWinner m

updateBoard :: [[Square]] -> Move -> Square -> [[Square]]
updateBoard m (Move _ outer inner) s = setListElem outer newInner  m
    where 
        innerBoard =  m !! outer
        newInner = setListElem (inner-1) s innerBoard      

getPlayer :: Game -> Move -> Square
getPlayer (Game x o _ _ _ _ _) (Move p _ _)
    | p == x = X
    | p == o = O

setListElem::Int -> a-> [a]->[a]
setListElem n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:setListElem (n - 1) newVal xs
