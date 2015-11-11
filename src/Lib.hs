module Lib where

import Game.Types
import Data.Matrix
import qualified Data.Vector as V
import Control.Monad.Trans.Except

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maybeToError err Nothing = throwE err
maybeToError _  (Just a) = pure a

--Use with matrix rows, columns, and diagonal as a list to determine if a player has won
checkWin :: Eq a => [V.Vector a] -> Bool
checkWin xs = any (== True) $ map (\v -> V.all (== V.head v) (V.tail v)) xs

