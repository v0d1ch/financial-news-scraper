{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.HTML.Freader
  ( parseXml
  , RssFeed(..)
  ) where

import           Control.Exception.Safe (Exception, SomeException (..), try)
import           Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text, unpack)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Text.XML (Document, def, parseLBS)
import           Text.XML.Cursor

type RssM = ExceptT String IO

data RssFeed = RssFeed
  { rssTitle :: Text
  , rssUrl   :: Text
  } deriving (Show)

data RssException =
  RssException String
  deriving (Show)

instance Exception RssException

parseRss :: ByteString -> RssM Document
parseRss bs = res
  where
    res =
      case parseLBS def bs of
        Left (SomeException a) -> throwError (show a)
        Right d                -> return d

getFeed :: Text -> RssM Document
getFeed rssUrl = do
  manager <- lift $ newManager $ managerSetProxy noProxy tlsManagerSettings
  lift $ setGlobalManager manager
  rssRequest <- parseRequest $ unpack rssUrl
  crumb <-
    lift $ try (httpLbs rssRequest manager) :: RssM (Either RssException (Response ByteString))
  case fmap responseBody crumb of
    Left (RssException e) -> throwError e
    Right a               -> parseRss a

-- rssLink = "http://feeds.reuters.com/reuters/businessNews"
parseXml :: Text -> IO [RssFeed]
parseXml url = do
  doc <- runExceptT (getFeed url)
  case doc of
    Left _ -> return [RssFeed {rssTitle = "", rssUrl = ""}]
    Right d -> do
      let cursor = fromDocument d
      let titles =
            child cursor >>= element "channel" >>= child >>= element "item" >>=
            child >>=
            element "title" >>=
            descendant >>=
            content
      let links =
            child cursor >>= element "channel" >>= child >>= element "item" >>=
            child >>=
            element "link" >>=
            descendant >>=
            content
      let joinData = Prelude.zip titles links
      return $ Prelude.map createFeed joinData

createFeed :: (Text, Text) -> RssFeed
createFeed l = RssFeed {rssTitle = fst l, rssUrl = snd l}
