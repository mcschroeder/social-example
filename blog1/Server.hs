{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time
import qualified Data.Text.Lazy as L
import Network.HTTP.Types.Status
import System.Locale
import System.Timeout
import Web.Scotty

import BlogDB
import TX

------------------------------------------------------------------------------

main :: IO ()
main = do
    db <- openDatabase "blog" =<< emptyBlogDB
    scotty 3000 $ do

        get "/users" $ do
            names <- liftIO $ runTX db getAllUserNames
            json names

        put "/users" $ do
            username <- param "name"
            user <- liftIO $ runTX db $ newUser username
            json $ object [ "name" .= name user ]

        get "/users/:name" $ do
            name <- param "name"
            user <- liftIO $ runTX db $ getUserJson name
            json user

        put "/users/:name1/following" $ do
            name1 <- param "name1"
            name2 <- param "name"
            liftIO $ runTX db $ do
                user1 <- getUser name1
                user2 <- getUser name2
                user1 `follow` user2
            status noContent204

        delete "/users/:name1/following" $ do
            name1 <- param "name1"
            name2 <- param "name"
            liftIO $ runTX db $ do
                user1 <- getUser name1
                user2 <- getUser name2
                user1 `unfollow` user2
            status noContent204

        get "/users/:name/posts" $ do
            name <- param "name"
            posts <- liftIO $ runTX db $ do
                user <- getUser name
                liftSTM $ readTVar (posts user)
            json posts

        get "/users/:name/feed" $ do
            name <- param "name"
            lastKnownTime <- param "lastKnownTime" `rescue` const next
            feed <- liftIO $ timeout (30 * 10^6) $ runTX db $ do
                user <- getUser name
                liftSTM $ waitForFeed user lastKnownTime
            maybe (status requestTimeout408) json feed

        get "/users/:name/feed" $ do
            name <- param "name"
            feed <- liftIO $ runTX db $ do
                user <- getUser name
                liftSTM $ feed user
            json feed

        post "/users/:name/posts" $ do
            name <- param "name"
            body <- param "body"
            liftIO $ runTX db $ do
                user <- getUser name
                newPost user body
            status noContent204

------------------------------------------------------------------------------

getAllUserNames :: TX BlogDB [Text]
getAllUserNames = do
    db <- getData
    liftSTM $ map fst . Map.toList <$> readTVar (users db)

getUserJson :: Text -> TX BlogDB Aeson.Value
getUserJson username = do
    user <- getUser username
    followers <- liftSTM $ Set.toList <$> readTVar (followers user)
    following <- liftSTM $ Set.toList <$> readTVar (following user)
    return $ object [ "name" .= name user
                    , "followers" .= map name followers
                    , "following" .= map name following
                    ]

instance Aeson.ToJSON Post where
    toJSON (Post author time body) = object [ "author" .= name author
                                            , "time" .= time
                                            , "body" .= body
                                            ]

instance Parsable UTCTime where
    parseParam = maybe (Left "no parse") Right
               . parseTime defaultTimeLocale "%FT%T%Q%Z"
               . L.unpack
