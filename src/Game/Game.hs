{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Game.Game where

import Servant.API
import Servant.Server.Internal.ServantErr
import Game.Types
import qualified Data.HashMap as H
import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Lib
import Control.Monad.Trans.Except
import qualified Data.Matrix as MTX

type GameEndpoints =      Capture "curPlayer" String :> Capture "oppPlayer" String :> Get '[JSON] Game
                     :<|> Capture "curPlayer" String :> Capture "oppPlayer" String :> Capture "outerMove" Integer :> Capture "innerMove" Integer :> Get '[JSON] Game
                     :<|> Capture "curPlayer" String :> Capture "oppPlayer" String :> Capture "command" String :> Get '[JSON] ()


gameServer keyValueStoreRef = createGame keyValueStoreRef :<|> doMove keyValueStoreRef :<|> command keyValueStoreRef


doMove store curPlayer oppPlayer inner outer = getFromStore store curPlayer oppPlayer >>= validateMove newMove >>= updateGame newMove >>= checkForWin 
    where
        newMove = (Move curPlayer outer inner)

getFromStore:: TVar (H.Map String Game) -> String -> String -> ExceptT ServantErr IO Game
getFromStore storeRef curPlayer oppPlayer = do
  store <- liftIO $ readTVarIO storeRef
  let findVar = H.lookup (curPlayer ++ oppPlayer) store <|> H.lookup (oppPlayer ++ curPlayer) store
  maybeToError custErr404 findVar
  where custErr404 = err404 {errBody = "couldn't find game"}

validateMove:: Move -> Game -> ExceptT ServantErr IO Game
validateMove move@(Move curPlayer outer inner) gameState@(Game _ _ (Move lastPlayer lastOuter lastInner) board _ _)
    -- the current player is the last player; move out of turn
    | curPlayer == lastPlayer = throwE err406
    -- the current outer square is not the last inner move; bad move
    | outer /= lastInner = throwE err406
    -- the looks at theouter move (where the player wants to go)
    --    and checks to see if the any of the cells are empty. it
    --    there are no empty cells, return an error.
    | (any (== Empty) $ MTX.toList targetBoard) == False = throwE err406
    -- look at the target inner board and see if the desired move is occupied
    | (MTX.toList targetBoard) !! (inner - 1) /= Empty = throwE err406
    | otherwise = pure gameState
    where
        targetBoard = MTX.toList board !! (outer - 1)

updateGame :: Move -> Game -> ExceptT ServantErr IO Game
updateGame move@(Move curPlayer _ _) game@(Game x o lastMove board meta moves) = pure Game {
        playerX = x,
        playerO = o,
        lastMove = move,
        board = newBoard,
        metaBoard = (checkSubBoards newBoard),
        moves = (moves + 1)
    }
    where
        newBoard = updateBoard board move nowPlayer
        nowPlayer = getPlayer game move

checkForWin = undefined

createGame store curPlayer oppPlayer = undefined

command store curPlayer playerY = undefined

