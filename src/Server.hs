{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Server where

import Servant.API
import Game.Game
import Account.Account
import System.Environment
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.JQuery
import Servant.Server
import Servant
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.HashMap as H
import Servant.Docs

type FullServer = "game" :> GameEndpoints :<|> "account" :> AccountEndpoints :<|> Raw

server gameRef = gameServer gameRef :<|> accountServer :<|> serveDirectory "./www/"

serverAPI::Proxy FullServer
serverAPI = Proxy

app gameRef = serve serverAPI $ server gameRef

mainFunc = do
  args <- getArgs
  if args == []
    then do
      gameRef <- atomically $ newTVar (H.empty)
      run 8080 (app gameRef)
    else useArg (args !! 0)

useArg:: String -> IO ()
useArg arg
  -- | (arg == "j") = writeFile "./api.js" js
  | (arg == "m") = makeDB
  -- | (arg == "d") = writeFile "./docs.html" doc
  | otherwise = print "Didn't understand that command line arg"

{--
doc::String
doc = markdown $ docs serverAPI

js::String
js=jsForAPI serverAPI
--}
