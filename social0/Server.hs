{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Applicative
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as Map
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, toJSON, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time
import qualified Data.Text.Lazy as L
import GHC.Conc.Sync (unsafeIOToSTM)
import Network.HTTP.Types.Status
import System.Timeout
import Web.Scotty hiding (body)

import SocialDB

------------------------------------------------------------------------------

main :: IO ()
main = do
    db <- emptySocialDB

    let liftSTM   = liftIO . atomically
        jsonSTM x = json =<< liftSTM x

    let jsonTimeoutSTM t x = do
            res <- liftIO $ timeout t $ atomically x
            maybe (status requestTimeout408) json res

    scotty 3000 $ do

        get "/users" $ do
            jsonSTM $ getAllUserNames db

        put "/users" $ do
            name <- param "name"
            jsonSTM $ userToJson =<< createUser name db

        get "/users/:name" $ do
            name <- param "name"
            jsonSTM $ userToJson =<< getUser name db

        put "/users/:name1/following" $ do
            name1 <- param "name1"
            name2 <- param "name"
            liftSTM $ do
                user1 <- getUser name1 db
                user2 <- getUser name2 db
                user1 `follow` user2

        delete "/users/:name1/following" $ do
            name1 <- param "name1"
            name2 <- param "name"
            liftSTM $ do
                user1 <- getUser name1 db
                user2 <- getUser name2 db
                user1 `unfollow` user2

        get "/users/:name/timeline" $ do
            name <- param "name"
            jsonSTM $ do
                user <- getUser name db
                readTVar (timeline user)

        get "/users/:name/feed" $ do
            name <- param "name"
            lastKnownTime <- param "lastKnownTime" `rescue` const next
            jsonTimeoutSTM (30 * 10^6) $ do
                user <- getUser name db
                waitForFeed user lastKnownTime

        get "/users/:name/feed" $ do
            name <- param "name"
            jsonSTM $ do
                user <- getUser name db
                feed user

        post "/users/:name/timeline" $ do
            name <- param "name"
            body <- param "body"
            liftSTM $ do
                user <- getUser name db
                void $ createPost user body db

------------------------------------------------------------------------------

getAllUserNames :: SocialDB -> STM [Text]
getAllUserNames db = do
    usermap <- unsafeIOToSTM $ Map.unsafeToList (users db)
    return $ map fst usermap

userToJson :: User -> STM Aeson.Value
userToJson user = do
    followers <- Set.toList <$> readTVar (followers user)
    following <- Set.toList <$> readTVar (following user)
    return $ object [ "name" .= name user
                    , "followers" .= map name followers
                    , "following" .= map name following
                    ]

instance ToJSON Post where
    toJSON post = object [ "postId" .= postId post
                         , "author" .= (name . author) post
                         , "time" .= time post
                         , "body" .= body post
                         ]

instance ToJSON PostId where
    toJSON (PostId pid) = toJSON $ show pid

instance Parsable PostId where
    parseParam = fmap PostId . readEither

instance Parsable UTCTime where
    parseParam = maybe (Left "no parse") Right
               . parseTimeM True defaultTimeLocale "%FT%T%Q%Z"
               . L.unpack
