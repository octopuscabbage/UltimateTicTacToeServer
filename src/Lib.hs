module Lib
    ( someFunc
    ) where
import Types
import Game
import Data.Matrix
import qualified Data.Vector as V
someFunc :: IO ()
someFunc = putStrLn "someFunc"


--Use with matrix rows, columns, and diagonal as a list to determine if a player has won
checkWin :: Square -> [V.Vector Square] -> Bool
checkWin s xs = any (== True) $ map (\v -> V.all (\x -> x == s || x == Both) v) xs

--Given a matrix of squares this will return which square (X, O, Neither, Both) has won
giveWinner :: Matrix Square -> Square
giveWinner m
	| ((checkWin X items) && (checkWin O items)) = Both
	| checkWin X items                           = X
	| checkWin O items                           = O
	| otherwise                                  = Empty
	where 
		rows = map (\x -> Data.Matrix.getRow x m) [1,2,3]
		columns = map (\x -> Data.Matrix.getCol x m) [1,2,3]
		leftDiag = [(Data.Matrix.getDiag m)]
		rightDiag = [V.fromList [(Data.Matrix.getElem 3 1 m), (Data.Matrix.getElem 2 2 m), (Data.Matrix.getElem 1 3 m)]]
		items = rows ++ columns ++ leftDiag ++ rightDiag

--Given a game board, return a meta board showing state of all sub-boards
checkSubBoards :: Matrix (Matrix Square) -> Matrix Square
checkSubBoards m = fmap giveWinner m