{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Game where

import Servant.API
import Servant.Server.Internal.ServantErr
import Types
import qualified Data.HashMap as H
import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

type GameEndpoints =      Capture "playerX" String :> Capture "playerO" String :> Get '[JSON] Game
                     :<|> Capture "playerX" String :> Capture "playerO" String :> Capture "outterMove" Integer :> Capture "innterMove" Integer :> Get '[JSON] Game
                     :<|> Capture "playerX" String :> Capture "playerO" String :> Capture "command" String :> Get '[JSON] ()


gameServer dbConnection keyValueStoreRef = createGame keyValueStoreRef :<|> doMove keyValueStoreRef :<|> command keyValueStoreRef


doMove store playerX playerO inner outer = getFromStore store playerX playerO >>= authMove >>= updateBoard >>= checkForWin 

getFromStore:: TVar (H.Map String Game) -> String -> String -> ExceptT ServantErr IO Game
getFromStore storeRef playerX playerO = do
  store <- liftIO $ readTVarIO storeRef
  let findVar = (H.lookup (playerX ++ playerO) store) <|> (H.lookup (playerO ++ playerX) store)
  case findVar of
   Nothing -> throwE $ err404 {errBody = "couldn't find game"}
   (Just a) -> pure a

authMove:: Game -> ExceptT ServantErr IO Game
authMove gameState = undefined

updateBoard = undefined

checkForWin = undefined

createGame store playerX playerO = undefined

command store playerX playerY = undefined

