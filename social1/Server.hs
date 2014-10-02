{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Monoid
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Time
import Text.Blaze.Html.Renderer.Text
import Text.Hamlet
import Web.Scotty
import Web.Scotty.Cookie

import SocialDB
import TX

userUrl :: User -> L.Text
userUrl user = "/" <> (L.fromStrict $ name user)

getLoggedInUser :: Database SocialDB -> ActionM (Maybe User)
getLoggedInUser db = do
    name <- getCookie "name"
    case name of
        Just name -> do
            user <- liftIO $ runTX db $ getUser name
            return $ Just user
        Nothing -> return Nothing

main :: IO ()
main = do
    db <- openDatabase "social" =<< emptySocialDB

    scotty 3000 $ do
        get "/style.css" $ file "./social1/style.css"

        get "/" $ do
            maybeViewer <- getLoggedInUser db
            users <- liftIO $ runTX db getAllUsers
            blaze $ banner "/" maybeViewer <> [shamlet|
                <div #allusers>
                    <h2>Users
                    #{userList users}
            |]

        post "/login" $ do
            name <- param "name"
            user <- liftIO $ runTX db $ getUser name
            setSimpleCookie "name" name
            jumpbackURL <- param "jumpback" `rescue` (const $ return $ userUrl user)
            redirect jumpbackURL

        post "/logout" $ do
            deleteCookie "name"
            jumpbackURL <- param "jumpback" `rescue` (const $ return "/")
            redirect jumpbackURL

        get "/register" $ do
            blaze [shamlet|
                <form action=/register method=post>
                    <input type=text name=name>
                    <input type=submit value="Register new user">
            |]

        post "/register" $ do
            name <- param "name"
            user <- liftIO $ runTX db $ newUser name
            redirect $ userUrl user

        get "/:name" $ do
            name <- param "name"
            maybeViewer <- getLoggedInUser db
            profile <- liftIO $ runTX db $ do
                user <- getUser name
                renderProfile maybeViewer user
            blaze profile

        get "/:name/post" $ do
            name <- param "name"
            body <- param "body"
            liftIO $ runTX db $ do
                user <- getUser name
                newPost user body
            redirect $ "/" <> (L.fromStrict name)

        get "/:author/:time/like" $ do
            author <- param "author"
            time <- param "time"
            user <- param "user"
            liftIO $ runTX db $ do
                user <- getUser user
                author <- getUser author
                post <- getPost author time
                user `like` post
            redirect $ "/" <> (L.fromStrict author)

        get "/:author/:time/unlike" $ do
            author <- param "author"
            time <- param "time"
            user <- param "user"
            liftIO $ runTX db $ do
                user <- getUser user
                author <- getUser author
                post <- getPost author time
                user `unlike` post
            redirect $ "/" <> (L.fromStrict author)


instance Parsable UTCTime where
    parseParam = readEither

blaze :: Html -> ActionM ()
blaze body = html $ renderHtml [shamlet|
    $doctype 5
    <html>
        <head>
            <link rel=stylesheet href=style.css>
        <body>
            #{body}
    |]

banner :: L.Text -> Maybe User -> Html
banner jumpbackUrl maybeViewer = [shamlet|
    <div #banner>
        <a href="/">The Funcbook
        <div .userinfo>
            $maybe viewer <- maybeViewer
                Logged in as <a href="#{userUrl viewer}">#{name viewer}</a>.
                <form action=/logout method=post>
                    <input type=hidden name=jumpback value=#{jumpbackUrl}>
                    <input type=submit value=Logout>
            $nothing
                <form action=/login method=post>
                    <input type=hidden name=jumpback value=#{jumpbackUrl}>
                    <input type=text name=name>
                    <input type=submit value=Login>
                <a href=/register>Register
    |]

renderProfile :: Maybe User -> User -> TX SocialDB Html
renderProfile maybeViewer user = do
    friends <- liftSTM $ Set.toList <$> readTVar (friends user)
    feed <- mapM (renderPost maybeViewer) =<< getFeed user
    return $ banner (userUrl user) maybeViewer <> [shamlet|
        <div #profile>
            <div .userinfo>
                <h1>#{name user}
                <div .friends>
                    #{length friends} friends
                    #{userList friends}
            <div .feed>
                $maybe viewer <- maybeViewer
                    $if viewer == user
                        <form .postform action="/#{name user}/post">
                            <textarea name=body>
                            <input type=submit value=Post>
                #{feed}
    |]

userList :: [User] -> Html
userList users = [shamlet|
    <ul .userlist>
        $forall user <- users
            <li><a href="/#{name user}">#{name user}
    |]

compactUserList :: [User] -> Html
compactUserList users = [shamlet|
    $if (not $ null users)
        $forall user <- init users
            <span><a href="/#{name user}">#{name user}</a>,
        <a href="/#{name $ last users}">#{name $ last users}</a>
    |]

renderPost :: Maybe User -> Post -> TX SocialDB Html
renderPost maybeViewer post = do
    likes <- liftSTM $ Set.toList <$> readTVar (likedBy post)
    return [shamlet|
        <div .post>
            <b .author>#{name $ author post}
            <i .time>#{show $ time post}
            <p .body>#{SocialDB.body post}
            <div .likes>
                $maybe viewer <- maybeViewer
                    $if elem viewer likes
                        <a href="#{postUrl post}/unlike?user=#{name viewer}">Unlike
                    $else
                        <a href="#{postUrl post}/like?user=#{name viewer}">Like
                $if not (null likes)
                    Liked by #{compactUserList likes}
    |]

postUrl :: Post -> T.Text
postUrl post = mconcat ["/", name $ author post, "/", T.pack $ show $ time post]

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
