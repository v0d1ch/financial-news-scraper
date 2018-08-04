{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fshow-hole-constraints #-}

module DB.Model where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Text
import Data.Time
import qualified Database.Persist as P
import Database.Persist.Postgresql
       (ConnectionString, runSqlPersistM, withPostgresqlConn, SqlPersistT)
import Database.Persist.Sql
import qualified Database.Persist.TH as PTH
import System.Environment (getEnv)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Story
    hashId Int
    title Text
    link Text
    content Text Maybe
    image Text Maybe
    created UTCTime
    deriving Eq Show
|]

instance ToJSON (P.Entity Story) where
    toJSON (P.Entity _ p) = object
        [ "title"   .= storyTitle p
        , "link"    .= storyLink p
        , "content" .= storyContent p
        , "image"   .= storyImage p
        ]

connStr :: IO String
connStr = do
  host <- getEnv "DATABASE_HOST"
  port <- getEnv "DATABASE_PORT"
  user <- getEnv "DATABASE_USER"
  password <- getEnv "DATABASE_PASSWORD"
  name <- getEnv "DATABASE_NAME"
  return $ mconcat ["host=", host, " port=", port, " dbname=", name , " user=", user, " password=", password]

migrateDB :: IO ()
migrateDB = runDb (runMigration migrateAll)

runDb  query = do
  cs <- BS.pack <$> connStr
  runStderrLoggingT $
    withPostgresqlConn cs $ \conn ->
      void $ liftIO $ flip runSqlPersistM conn $ query

