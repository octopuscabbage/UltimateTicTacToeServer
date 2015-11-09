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
checkWin :: Eq a => [V.Vector a] -> Bool
checkWin xs = any (== True) $ map (\v -> V.all (== V.head v) (V.tail v)) xs
