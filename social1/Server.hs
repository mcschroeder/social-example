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
import TX

------------------------------------------------------------------------------

main :: IO ()
main = do
    db <- openDatabase "social.db" =<< emptySocialDB

    let liftTX   = liftIO . durably db
        jsonTX x = json =<< liftTX x

    let jsonTimeoutTX t x = do
            res <- liftIO $ timeout t $ durably db x
            maybe (status requestTimeout408) json res

    scotty 3000 $ do

        get "/users" $ do
            jsonTX getAllUserNames

        put "/users" $ do
            name <- param "name"
            jsonTX $ userToJson =<< createUser name

        get "/users/:name" $ do
            name <- param "name"
            jsonTX $ userToJson =<< getUser name

        put "/users/:name1/following" $ do
            name1 <- param "name1"
            name2 <- param "name"
            liftTX $ do
                user1 <- getUser name1
                user2 <- getUser name2
                user1 `follow` user2

        delete "/users/:name1/following" $ do
            name1 <- param "name1"
            name2 <- param "name"
            liftTX $ do
                user1 <- getUser name1
                user2 <- getUser name2
                user1 `unfollow` user2

        get "/users/:name/timeline" $ do
            name <- param "name"
            jsonTX $ do
                user <- getUser name
                liftSTM $ readTVar (timeline user)

        get "/users/:name/feed" $ do
            name <- param "name"
            lastKnownTime <- param "lastKnownTime" `rescue` const next
            jsonTimeoutTX (30 * 10^6) $ do
                user <- getUser name
                liftSTM $ waitForFeed user lastKnownTime

        get "/users/:name/feed" $ do
            name <- param "name"
            jsonTX $ do
                user <- getUser name
                liftSTM $ feed user

        post "/users/:name/timeline" $ do
            name <- param "name"
            body <- param "body"
            liftTX $ do
                user <- getUser name
                void $ createPost user body

------------------------------------------------------------------------------

getAllUserNames :: TX SocialDB [Text]
getAllUserNames = do
    db <- getData
    usermap <- unsafeIOToTX $ Map.unsafeToList (users db)
    return $ map fst usermap

userToJson :: User -> TX SocialDB Aeson.Value
userToJson user = liftSTM $ do
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
