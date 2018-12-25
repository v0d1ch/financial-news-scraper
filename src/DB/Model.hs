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
import Data.Hashable
import Data.Either
import Data.Text (Text)
import Data.Time
import Data.ByteString.Char8 as BS
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

connStr :: IO ConnectionString
connStr = do
  host <- getEnv "DB_HOST"
  port <- getEnv "DB_PORT"
  user <- getEnv "DB_USER"
  password <- getEnv "DB_PASSWORD"
  name <- getEnv "DB_DATABASE"
  return $ BS.pack $ mconcat ["host=", host, " port=", port, " dbname=", name , " user=", user, " password=", password]

migrateDB :: IO ()
migrateDB = runDb (runMigration migrateAll)

runDb :: ReaderT SqlBackend IO b -> IO b
runDb query = do
  pool <- createPool
  runSqlPool query pool

createPool :: IO ConnectionPool
createPool =
  -- dbconf <- loadDbConf
  -- createPoolConfig dbconf
  createPoolSimple

createPoolSimple :: IO ConnectionPool
createPoolSimple = do
  cstr <- connStr
  let cfg = PostgresConf {pgConnStr = cstr, pgPoolSize = 10}
  createPoolConfig cfg

loadDbConf :: IO PostgresConf
loadDbConf = do
  yaml <- decodeFileEither "config/postgresql.yaml"
  dbconf <- parseMonad loadConfig (fromRight "" yaml)
  applyEnv (dbconf :: PostgresConf)

makeHash
  :: Hashable a
  => a -> Int
makeHash = hash
