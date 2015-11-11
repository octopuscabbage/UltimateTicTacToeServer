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
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

{--
 IGNORRE THE FOLLOWING TEMPLATE HASKELL BS
 USING THIS IS BETTER THAN ACTUALLY WRITING DB STUFF
 BUT IT'S REALLY UNIDIOMATIC
 AND FRAGILE
--}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account
    name String
    wins Int
    losses Int
    UniqueName name
    deriving Show
|]

