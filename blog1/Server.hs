{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Web.Scotty

import BlogDB
import TX

main :: IO ()
main = do
    db <- openDatabase "blog" =<< emptyBlogDB

    let tx = liftIO . runTX db

    scotty 3000 $ do

        get "/users" $ do
            json =<< tx getAllUserNames

        put "/users" $ do
            username <- param "name"
            user <- tx $ newUser username
            json $ object [ "name" .= name user ]

        get "/users/:name" $ do
            name <- param "name"
            json =<< tx (getUserJson name)

        put "/users/:name1/following" $ do
            name1 <- param "name1"
            name2 <- param "name"
            tx $ do
                user1 <- getUser name1
                user2 <- getUser name2
                user1 `follow` user2
            json ()

        delete "/users/:name1/following" $ do
            name1 <- param "name1"
            name2 <- param "name"
            tx $ do
                user1 <- getUser name1
                user2 <- getUser name2
                user1 `unfollow` user2
            json ()

        get "/users/:name/posts" $ do
            name <- param "name"
            posts <- tx $ do
                user <- getUser name
                liftSTM $ readTVar (posts user)
            json posts

        get "/users/:name/feed" $ do
            name <- param "name"
            feed <- tx $ do
                user <- getUser name
                liftSTM $ feed user
            json feed

        post "/users/:name/posts" $ do
            name <- param "name"
            body <- param "body"
            tx $ do
                user <- getUser name
                newPost user body
            json ()

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
