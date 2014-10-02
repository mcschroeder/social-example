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
import Data.Text.Lazy (Text)
import Data.Time
import Text.Blaze.Html.Renderer.Text
import Text.Hamlet
import Web.Scotty
import Web.Scotty.Cookie

import BlogDB
import TX

main :: IO ()
main = do
    db <- openDatabase "blog" =<< emptyBlogDB

    let tx = liftIO . runTX db

    let getViewer = do
            name <- getCookie "name"
            case name of
                Just name -> Just <$> tx (getUser name)
                Nothing -> return Nothing

    scotty 3000 $ do

        get "/style.css" $ do
            file "./blog1/style.css"

        get "/" $ do
            viewer <- getViewer
            users <- tx $ do
                db <- getData
                usermap <- liftSTM $ readTVar (users db)
                return $ map snd $ Map.toList usermap

            blaze $ banner "/" viewer <> [shamlet|
                $forall user <- users
                    <a href="#{userUrl user}">#{name user}
                    <br>
            |]

        get "/register" $ do
            blaze $ banner "/" Nothing <> [shamlet|
                <form action=/register method=post>
                    <input type=text name=name>
                    <input type=submit value="Register new user">
            |]

        post "/register" $ do
            name <- param "name"
            user <- tx $ newUser name
            setSimpleCookie "name" name
            redirect $ userUrl user

        post "/login" $ do
            name <- param "name"
            user <- tx $ getUser name
            setSimpleCookie "name" name
            jumpbackUrl <- param "jumpback"
                            `rescue` (const $ return $ userUrl user)
            redirect jumpbackUrl

        post "/logout" $ do
            deleteCookie "name"
            jumpbackUrl <- param "jumpback"
                            `rescue` (const $ return $ "/")
            redirect jumpbackUrl

        get "/:name" $ do
            name <- param "name"
            viewer <- getViewer
            page <- tx $ profile viewer =<< getUser name
            blaze $ banner ("/" <> (L.fromStrict name)) viewer <> page

        post "/follow" $ do
            name <- param "name"
            (Just viewer) <- getViewer
            tx $ follow viewer =<< getUser name
            jumpbackUrl <- param "jumpback"
                            `rescue` (const $ return $ userUrl viewer)
            redirect jumpbackUrl

        post "/unfollow" $ do
            name <- param "name"
            (Just viewer) <- getViewer
            tx $ unfollow viewer =<< getUser name
            jumpbackUrl <- param "jumpback"
                            `rescue` (const $ return $ userUrl viewer)
            redirect jumpbackUrl

blaze :: Html -> ActionM ()
blaze body = html $ renderHtml [shamlet|
    $doctype 5
    <html>
        <head>
            <link rel=stylesheet href=style.css>
        <body>
            #{body}
    |]

banner :: Text -> Maybe User -> Html
banner jumpbackUrl maybeViewer = [shamlet|
     <div #banner>
        <a href="/">µSpace
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

userUrl :: User -> Text
userUrl user = "/" <> L.fromStrict (name user)

profile :: Maybe User -> User -> TX BlogDB Html
profile maybeViewer user = do
    following <- liftSTM $ readTVar (following user)
    followers <- liftSTM $ readTVar (followers user)
    return [shamlet|
        <div .profile>
            <h2>#{name user}
            $maybe viewer <- maybeViewer
                $if Set.member viewer followers
                    <form .follow action=/unfollow method=post>
                        <input type=hidden name=name value="#{name user}">
                        <input type=hidden name=jumpback value="#{userUrl user}">
                        <input type=submit value="Unfollow">
                $elseif viewer /= user
                    <form .follow action=/follow method=post>
                        <input type=hidden name=name value="#{name user}">
                        <input type=hidden name=jumpback value="#{userUrl user}">
                        <input type=submit value="Follow">
            <h3>Following:
            $if Set.null following
                none
            $else
                $forall f <- Set.toList following
                    <a href="#{userUrl f}">#{name f}
                    <br>
            <h3>Followers:
            $if Set.null followers
                none
            $else
                $forall f <- Set.toList followers
                    <a href="#{userUrl f}">#{name f}
                    <br>
    |]
