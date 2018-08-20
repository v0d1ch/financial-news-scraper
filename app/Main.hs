{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text
import Data.Time
import Database.Persist
import DB.Model
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request, Response, pathInfo, requestMethod, responseLBS,
                    strictRequestBody)
import Network.Wai.Handler.Warp (run)
import qualified Text.HTML.Freader as R
import qualified Text.HTML.Fscraper as F

main :: IO ()
main = do
  let port = 5000
  putStrLn $ "Listening on port " ++ show port
  void $ insertStoriesReuters
  run port app

app :: Application
app req f =
  f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

insertStoriesReuters :: IO ()
insertStoriesReuters = do
  now <- liftIO getCurrentTime
  topnews <- getTopStory
  fnews <- getFeatureStories
  snews <- getSideStories
  rssnews <- liftIO $ R.parseXml "http://feeds.reuters.com/reuters/businessNews"
  let topstories = mapM convertImageStory topnews now
      fstories = mapM convertImageStory fnews now
      sstories = mapM convertStory snews now
      rssstories = mapM convertRssFeed rssnews now
      allS = topstories <> fstories <> sstories <> rssstories
  void $ mapM checkStorySaved allS


checkStorySaved :: Story -> IO (Maybe (Entity Story))
checkStorySaved story = do
  insertedStory <- runDb $ selectFirst [StoryHashId ==. storyHashId story] []
  case insertedStory of
    Nothing -> do
      _ <- runDb $ insert story
      return Nothing
    Just s -> return $ Just s

getTopStory :: MonadIO m => m [F.News]
getTopStory = do
  headStory <- liftIO $ F.topStory "olympics-topStory" F.reutersUrl
  case headStory of
    Nothing -> return []
    Just a -> return a


getFeatureStories :: MonadIO m => m [F.News]
getFeatureStories = do
  stories <- liftIO $ F.featureNews "column1" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a


getSideStories :: MonadIO m => m [F.News]
getSideStories = do
  stories <- liftIO $ F.leftColumnNews "more-headlines" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a

convertImageStory :: F.News -> UTCTime -> Story
convertImageStory news now =
  Story
  { storyHashId = makeHash (F.newstitle news)
  , storyTitle = pack $ F.newstitle news
  , storyLink = pack $ F.newslink news
  , storyContent = Just (pack $ F.newstext news)
  , storyImage = Just (pack $ F.newsimage news)
  , storyCreated = now
  }


convertStory :: F.News -> UTCTime -> Story
convertStory news now =
  Story
  { storyHashId = makeHash (F.newstitle news)
  , storyTitle = pack $ F.newstitle news
  , storyLink = pack $ F.newslink news
  , storyContent = Just (pack $ F.newstext news)
  , storyImage = Nothing
  , storyCreated = now
  }

convertRssFeed :: R.RssFeed -> UTCTime -> Story
convertRssFeed feed now =
  Story
  { storyHashId = makeHash (R.rssTitle feed)
  , storyTitle = R.rssTitle feed
  , storyLink = R.rssUrl feed
  , storyContent = Nothing
  , storyImage = Nothing
  , storyCreated = now
  }
