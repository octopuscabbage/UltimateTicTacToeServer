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
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Lib
import Control.Monad.Trans.Except
import qualified Data.Matrix as MTX
import Data.Maybe

type GameEndpoints =      Capture "curPlayer" String :> Capture "oppPlayer" String :> "new":> Get '[JSON] Game
                          :<|> Capture "curPlayer" String :> Capture "oppPlayer" String :> "view" :> Get '[JSON] Game
                          :<|> Capture "curPlayer" String :> Capture "oppPlayer" String :> Capture "outerMove" Integer :> Capture "innerMove" Integer :> Get '[JSON] Game

type Store = TVar (H.Map String Game) 
type ServantFunc a = ExceptT ServantErr IO a

gameServer keyValueStoreRef = createGame keyValueStoreRef
                              :<|> getFromStore keyValueStoreRef
                              :<|> doMove keyValueStoreRef


doMove store curPlayer oppPlayer inner outer = getFromStore store curPlayer oppPlayer >>= validateMove newMove >>= updateGame newMove >>= checkForWin 
    where
        newMove = (Move curPlayer outer inner)

getFromStore:: Store -> String -> String -> ServantFunc Game
getFromStore storeRef curPlayer oppPlayer = findGame storeRef curPlayer oppPlayer >>= maybeToError err404 {errBody = "Couldn't find game"}

validateMove:: Move -> Game -> ExceptT ServantErr IO Game
validateMove move@(Move curPlayer outer inner) gameState@(Game _ _ (Move lastPlayer lastOuter lastInner) board _ _ gameWon)
    | gameWon == Empty = pure gameState
    | lastPlayer == "none" = pure gameState
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
updateGame move@(Move curPlayer _ _) game@(Game x o lastMove board meta moves _) = pure Game {
        playerX = x,
        playerO = o,
        lastMove = move,
        board = newBoard,
        metaBoard = newMeta,
        moves = (moves + 1),
        gameWon = won
    }
    where
        newBoard = updateBoard board move nowPlayer
        nowPlayer = getPlayer game move
        newMeta = checkSubBoards newBoard
        won = giveWinner newMeta

checkForWin = undefined

createGame:: TVar (H.Map String Game) -> String -> String -> ExceptT ServantErr IO Game
createGame storeRef curPlayer oppPlayer =do
  store <- getStore storeRef
  game <- findGame storeRef curPlayer oppPlayer
  if isNothing game
    then (liftIO $ atomically $ writeTVar storeRef $ H.insert (curPlayer ++ oppPlayer) nGame store) >> pure nGame
    else throwE err400
    where nGame = newGame curPlayer oppPlayer

findGame:: Store -> String -> String -> ServantFunc (Maybe Game)
findGame storeRef curPlayer oppPlayer= do
  store <- getStore storeRef
  pure (H.lookup (curPlayer ++ oppPlayer) store <|> H.lookup (oppPlayer ++ curPlayer) store)

getStore:: Store -> ServantFunc (H.Map String Game)
getStore storeRef  = liftIO $ readTVarIO storeRef
