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

type Server = GameEndpoints :<|> AccountEndpoints 

server = gameServer :<|> accountServer
