{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Account.Account where

import Control.Monad.IO.Class 
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Servant.API
import Servant
import Control.Monad.Trans.Except
import Control.Applicative
import Control.Monad.Trans.Either
import Lib

import Account.Types

type AccountEndpoints = "create" :> Capture "name" String :> Get '[JSON] (Key Account)
                        :<|> "getAll" :> Get '[JSON] [Account]
                        :<|> "display" :> Capture "name" String :> Get '[JSON] Account

accountServer:: Server AccountEndpoints
accountServer =  newAccountEndpoint :<|>  getAllEndpoint :<|> displayEndpoint

--newAccountEndpoint:: String -> ExceptT ServantErr IO (Key Account)
newAccountEndpoint name = liftIO $ newAccount $ blankAccount name

--getAllEndpoint:: ExceptT ServantErr IO [Account]
getAllEndpoint = liftIO $  getAll

displayEndpoint::String -> EitherT ServantErr IO Account
displayEndpoint name = do
  accountEntity <- liftIO $ getAccountByName name
  entity <- getValue accountEntity
  let account = entityVal entity
  pure account
    where custErr404 = err404 {errBody = "Couldn't find that account"}
          getValue Nothing = left custErr404
          getValue (Just a) = right $ a

blankAccount name = Account name 0 0

dbLocation = "./accountdb"

makeDB:: IO ()
makeDB = runSqlite dbLocation $ runMigration migrateAll

newAccount account =  runSqlite dbLocation $ insert account

getAccountByName:: String -> IO (Maybe (Entity Account))
getAccountByName name = runSqlite dbLocation $ getBy $ UniqueName name

increaseWon:: String -> IO ()
increaseWon name = runSqlite dbLocation $ updateWhere [AccountName ==. name] [AccountWins +=. 1]

increaseLost:: String -> IO ()
increaseLost name = runSqlite dbLocation $ updateWhere [AccountName ==. name] [AccountLosses +=. 1]

getAll:: IO [Account]
getAll = runSqlite dbLocation $ do
             entity_list <- selectList [] []
             let list = map entityVal entity_list
             pure list

deleteAccount:: String -> IO ()
deleteAccount name = runSqlite dbLocation $ deleteWhere [AccountName ==. name] 
