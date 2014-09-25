{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Text.Blaze.Html.Renderer.Text
import Text.Hamlet
import Web.Scotty

import SocialDB
import TX

main :: IO ()
main = do
    db <- openDatabase "social" =<< emptySocialDB

    scotty 3000 $ do
        get "/" $ do
            users <- liftIO $ runTX db getAllUsers
            html $ renderHtml [shamlet|
                <h1>Welcome to Funcbook
                Register new user: <form action="/register">
                    <input type=text name=name>
                    <input type=submit>
                #{userList users}
            |]

        get "/register" $ do
            name <- param "name"
            user <- liftIO $ runTX db $ newUser name
            redirect $ "/" <> (L.fromStrict name)

        get "/:name" $ do
            name <- param "name"
            profile <- liftIO $ runTX db $ renderProfile =<< getUser name
            html $ renderHtml profile

        get "/:name/post" $ do
            name <- param "name"
            body <- param "body"
            liftIO $ runTX db $ do
                user <- getUser name
                newPost user body
            redirect $ "/" <> (L.fromStrict name)

renderProfile :: User -> TX SocialDB Html
renderProfile user = do
    friends <- liftSTM $ Set.toList <$> readTVar (friends user)
    feed <- mapM renderPost =<< getFeed user
    return [shamlet|
        <h1>#{name user}
        <h2>Friends
        #{userList friends}
        <h2>Feed
        <form action="/#{name user}/post">
            <textarea name=body>
            <input type=submit>
        #{feed}
    |]

userList :: [User] -> Html
userList users =
    [shamlet|
        <ul>
            $forall user <- users
                <li>#{linkify user}
    |]

compactUserList :: [User] -> Html
compactUserList = mconcat . intersperse ", " . map linkify

linkify :: User -> Html
linkify user = [shamlet| <li><a href=/#{name user}>#{name user} |]

renderPost :: Post -> TX SocialDB Html
renderPost Post{..} = do
    likes <- liftSTM $ Set.toList <$> readTVar likedBy
    return [shamlet|
        <div>
            <b>#{name author}
            <i>#{show time}
            <p>#{body}
            Liked by #{compactUserList likes}
    |]


getAllUsers :: TX SocialDB [User]
getAllUsers = do
    db <- getData
    liftSTM $ map snd . Map.toAscList <$> readTVar (users db)

getFeed :: User -> TX SocialDB [Post]
getFeed user = do
    myPosts <- getAllPosts user
    friends <- liftSTM $ Set.toList <$> readTVar (friends user)
    friendPosts <- concat <$> mapM getAllPosts friends
    return $ sortBy (flip $ comparing time) (myPosts ++ friendPosts)

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
