{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Game where

import Servant.API

type GameEndpoints =      Capture "playerX" String :> Capture "playerY" String 
                     :<|> Capture "playerX" String :> Capture "playerY" String :> Capture "outterMove" Integer :> Capture "innterMove" Integer
                     :<|> Capture "playerX" String :> Capture "playerY" String :> Capture "command" String
