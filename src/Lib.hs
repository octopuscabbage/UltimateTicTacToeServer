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

updateBoard :: Matrix (Matrix Square) -> Move -> Square -> Matrix (Matrix Square)
updateBoard m (Move _ outer inner) s = setElem newInner ((div outer 3), (mod outer 3)) m
    where 
        innerBoard = getElem (div outer 3) (mod outer 3) m
        newInner = setElem s ((div inner 3), (mod inner 3)) innerBoard      

updateGame :: Game -> Move -> Game
updateGame gameBoard@(Game x o l b mB moveCount) move = Game {playerX = x, playerO = o, lastMove = move, board = newBoard, metaBoard = (checkSubBoards newBoard), moves = (moveCount + 1)}
    where
        newBoard = updateBoard b move player
        player = getPlayer gameBoard move

getPlayer :: Game -> Move -> Square
getPlayer (Game x o _ _ _ _) (Move p _ _)
    | p == x = X
    | p == o = O

isSubBoardFull :: Matrix Square -> Bool
isSubBoardFull m
    | (any (== Empty) (toList m)) = False
    | otherwise = True

validateMove :: Game -> Move -> Bool
validateMove (Game _ _ (Move lastPlayer lastOuter lastInner) board _ _) (Move curPlayer outer inner)
    -- the current player is the last player; move out of turn
    | curPlayer == lastPlayer = False
    -- the current outer square is not the last inner move; bad start
    | outer     /= lastInner  = False
    -- look inside the current board and see if the square is full
--    | (any (== False) $ fmap (any (== Empty)) $ toList (fmap toList m) =  
    -- this looks at the outer move (where the player wants to go)
    --    and checks to see if any of the cells are empty. if there
    --    are no empty cells, return False, meaning the board is full
    | (any (== Empty) $ toList targetBoard) == False = False
    -- look at the target inner board and see if the desired move is occupied
    | (toList targetBoard) !! (inner - 1) /= Empty = False
    | otherwise = True
    where
        targetBoard = toList board !! (outer - 1)
