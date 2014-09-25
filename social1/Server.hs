{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.List
import Data.Monoid
import qualified Data.Map as Map
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Time
import Web.Scotty

import SocialDB
import TX

main :: IO ()
main = do
    db <- openDatabase "social" =<< emptySocialDB

    scotty 3000 $ do
        get "/" $ do
            users <- liftIO $ runTX db getAllUsers
            html $ "<h1>Welcome to Funcbook!</h1>"
                   <> renderUserList users

        get "/register/:name" $ do
            name <- param "name"
            user <- liftIO $ runTX db $ newUser $ L.toStrict name
            redirect $ "/" <> name

        get "/:name" $ do
            name <- param "name"
            page <- liftIO $ runTX db $ do
                user <- getUser $ L.toStrict name
                renderPage user
            html page


renderPage :: User -> TX SocialDB L.Text
renderPage user = do
    friends <- liftSTM $ Set.toList <$> readTVar (friends user)
    feed <- renderFeed =<< getFeed user
    return $ mconcat
        [ "<html>"
        , "<head><title>", L.fromStrict $ name user, "</title></head>"
        , "<body><h1>", L.fromStrict $ name user, "</h1>"
        , "<h3>Friends</h3>", renderUserList friends
        , "<h3>Feed</h3>", feed
        , "</html>"
        ]

renderUserList :: [User] -> L.Text
renderUserList users =
    mconcat ["<ul>", L.concat (map listify users), "</ul>"]
  where
    listify u = mconcat ["<li>", linkify u, "</li>"]

renderCompactUserList :: [User] -> L.Text
renderCompactUserList = L.intercalate ", " . map linkify

linkify :: User -> L.Text
linkify u = mconcat ["<a href='/", n, "'>", n, "</a>"]
  where n = L.fromStrict $ name u


renderFeed :: [Post] -> TX SocialDB L.Text
renderFeed posts = L.concat <$> mapM renderPost posts

renderPost :: Post -> TX SocialDB L.Text
renderPost Post{..} = do
    likes <- liftSTM $ Set.toList <$> readTVar likedBy
    return $ mconcat
        [ "<div class='post'>"
        , "<b>", L.fromStrict $ name author, "</b>", "<br>"
        , "<i>", renderTime time, "</i>", "<br>"
        , "<p>", L.fromStrict body, "</p>"
        , "Liked by ", renderCompactUserList likes
        , "</div>"
        ]

renderTime :: UTCTime -> L.Text
renderTime time = L.pack (show time)


getAllUsers :: TX SocialDB [User]
getAllUsers = do
    db <- getData
    liftSTM $ map snd . Map.toAscList <$> readTVar (users db)

getFeed :: User -> TX SocialDB [Post]
getFeed user = do
    myPosts <- getAllPosts user
    friends <- liftSTM $ Set.toList <$> readTVar (friends user)
    friendPosts <- concat <$> mapM getAllPosts friends
    return $ sortBy (comparing time) (myPosts ++ friendPosts)

getAllPosts :: User -> TX SocialDB [Post]
getAllPosts user =
    liftSTM $ map snd <$> Map.toDescList <$> readTVar (posts user)


-- waitForLikes :: Post -> TX SocialDB ()
-- waitForLikes post = do
--     likes <- liftSTM $ readTVar <$> likedBy post
--     check (notNull likes)
--     if null likes
--         then liftSTM $ retry
--         else becomeFriends (author post) (head likes)


-- friendBot :: Post -> IO ()
-- friendBot post = forever $ runTX db (waitForLikes post)
