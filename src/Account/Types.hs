{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Account.Types where
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

dbLocation = "./accountdb"
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account
    name String
    id String
    wins Int
    losses Int
    UniqueName name
    UniqueID id
    deriving Show
|]

runInDB f = runSqlite dbLocation f

migrate = runMigration migrateAll

newAccount account = runInDB $ insert account

getAccountByName name = runInDB $ getBy $ UniqueName name

getAccountByID id = runInDB $ getBy $ UniqueID id

idWon id = runInDB $ updateWhere [AccountId ==. id] [AccountWins +=. 1]


idLost id = runInDB $ updateWhere [AccountId ==. id] [AccountLosses +=. 1]



deleteAccount account = runInDB $ delete 
