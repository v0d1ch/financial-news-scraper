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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fshow-hole-constraints #-}

module DB.Model where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson
import Data.Either
import Data.Text (Text)
import Data.Time
import Data.Yaml
import qualified Database.Persist as P
import Database.Persist.Postgresql
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

runDb :: ReaderT SqlBackend IO b -> IO b
runDb query = do
  pool <- createPool
  runSqlPool query pool

createPool :: IO ConnectionPool
createPool = do
  dbconf <- loadDbConf
  createPoolConfig dbconf

createPoolSimple :: IO ConnectionPool
createPoolSimple = do
  let cfg =
        PostgresConf
        { pgConnStr = "host=localhost port=5432 dbname=ii connect_timeout=10"
        , pgPoolSize = 10
        }
  createPoolConfig cfg

loadDbConf :: IO PostgresConf
loadDbConf = do
  yaml <- decodeFileEither "config/postgresql.yaml"
  dbconf <- parseMonad loadConfig (fromRight "" yaml)
  applyEnv (dbconf :: PostgresConf)
