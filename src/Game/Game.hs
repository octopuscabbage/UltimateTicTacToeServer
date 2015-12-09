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
import qualified Data.Matrix as MTX
import Data.Maybe
import Servant
import Control.Monad.Trans.Either
import Account.Account

type GameEndpoints =      Capture "curPlayer" String :> Capture "oppPlayer" String :> "new":> Get '[JSON] Game
                          :<|> Capture "curPlayer" String :> Capture "oppPlayer" String :> "view" :> Get '[JSON] Game
                          :<|> Capture "curPlayer" String :> Capture "oppPlayer" String :> Capture "outerMove" Int :> Capture "innerMove" Int :> Get '[JSON] Game
                          :<|> "all" :> Get '[JSON] [String]

type Store = TVar (H.Map String Game) 
type ServantFunc a = EitherT ServantErr IO a

gameServer::Store -> Server GameEndpoints
gameServer keyValueStoreRef = createGame keyValueStoreRef
                              :<|> getFromStore keyValueStoreRef
                              :<|> doMove keyValueStoreRef
                              :<|> listGames keyValueStoreRef

doMove:: Store -> String -> String -> Int -> Int -> ServantFunc Game
doMove store curPlayer oppPlayer inner outer = getFromStore store curPlayer oppPlayer >>= validateMove newMove >>= updateGame newMove >>= updateStoreAndDB curPlayer oppPlayer store
    where
        newMove = (Move curPlayer outer inner)

updateStoreAndDB curPlayer oppPlayer storeRef gameState@(Game _ _ _ _ _ _ Empty) = do --No winner yet
  store <- getStore storeRef
  let newStore = if H.member (curPlayer++oppPlayer) store then H.adjust (const gameState) (curPlayer++oppPlayer) store else H.adjust (const gameState) (oppPlayer ++ curPlayer) store 
  liftIO $ atomically $ writeTVar storeRef newStore
  pure gameState
updateStoreAndDB curPlayer oppPlayer storeRef state = do
  store <- getStore storeRef
  liftIO $ increaseWon curPlayer
  liftIO $ increaseLost oppPlayer
  if H.member (curPlayer ++ oppPlayer) store then do
    liftIO $ atomically $ writeTVar storeRef $ H.delete (curPlayer ++ oppPlayer) store
    pure state
    else do
    liftIO $ atomically $ writeTVar storeRef $ H.delete(oppPlayer ++ curPlayer) store
    pure state

  

getFromStore:: Store -> String -> String -> ServantFunc Game
getFromStore storeRef curPlayer oppPlayer = findGame storeRef curPlayer oppPlayer >>= maybeToError err404 {errBody = "Couldn't find game"}

validateMove:: Move -> Game -> ServantFunc Game
validateMove move@(Move curPlayer outer inner) gameState@(Game _ _ (Move lastPlayer lastOuter lastInner) board _ _ gameWon)
    | outsideBounds outer  = left err400 {errBody ="Bad outer move"}
    | outsideBounds inner = left err400 {errBody = "Bad inner move"}
    | gameWon /= Empty = left err410 {errBody = "Game already won"}
    | lastPlayer == "None" = pure gameState
    -- the current player is the last player; move out of turn
    | curPlayer == lastPlayer = left err401 {errBody = "Move done out of turn, must wait for other player"}
    -- the current outer square is not the last inner move; bad move
    | outer /= lastInner = left err403 {errBody = "Current suqare is not the last inner move, bad move"}
    -- the looks at theouter move (where the player wants to go)
    --    and checks to see if the any of the cells are empty. it
    --    there are no empty cells, return an error.
    | (any (== Empty) $ targetBoard) == False = left err404 {errBody = "No empty cells in that square"}
    -- look at the target inner board and see if the desired move is occupied
    | (targetBoard) !! (inner - 1) /= Empty = left err409 {errBody = "Desired space is occupied"}
    | otherwise = pure gameState
    where
        targetBoard = board !! (outer - 1)
        outsideBounds x = x > 9 || x < 1

updateGame :: Move -> Game -> ServantFunc Game
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

createGame:: TVar (H.Map String Game) -> String -> String -> ServantFunc Game
createGame storeRef curPlayer oppPlayer =do
  store <- getStore storeRef
  game <- findGame storeRef curPlayer oppPlayer
  if isNothing game
    then (liftIO $ atomically $ writeTVar storeRef $ H.insert (curPlayer ++ oppPlayer) nGame store) >> pure nGame
    else left err400 {errBody = "Game already exists"}
    where nGame = newGame curPlayer oppPlayer

findGame:: Store -> String -> String -> ServantFunc (Maybe Game)
findGame storeRef curPlayer oppPlayer= do
  store <- getStore storeRef
  pure (H.lookup (curPlayer ++ oppPlayer) store <|> H.lookup (oppPlayer ++ curPlayer) store)

getStore:: Store -> ServantFunc (H.Map String Game)
getStore storeRef  = liftIO $ readTVarIO storeRef

listGames::Store -> ServantFunc [String]
listGames storeRef = H.keys <$> getStore storeRef
