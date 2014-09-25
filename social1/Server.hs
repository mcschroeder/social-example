{-# LANGUAGE OverloadedStrings #-}

module Server where

import Data.Monoid
import Web.Scotty

import SocialDB
import TX

main :: IO ()
main = scotty 3000 $ do
    get "/funcbook" $ do
        html $ "<h1>Funcbook</h2>"


-- getFeed :: User -> TX SocialDB [Post]
-- getFeed user = do
--     myPosts <- getAllPosts user
--     friends <- Set.toList <$> readTVar (friends user)
--     friendPosts <- concat <$> mapM getAllPosts friends
--     return $ sortBy (comparing `on` fst) (myPosts ++ friendPosts)

-- getAllPosts :: User -> TX SocialDB [Post]
-- getAllPosts user =
--     liftSTM $ map snd <$> Map.toDescList <$> readTVar (posts user)


-- waitForLikes :: Post -> TX SocialDB ()
-- waitForLikes post = do
--     likes <- liftSTM $ readTVar <$> likedBy post
--     check (notNull likes)
--     if null likes
--         then liftSTM $ retry
--         else becomeFriends (author post) (head likes)


-- friendBot :: Post -> IO ()
-- friendBot post = forever $ runTX db (waitForLikes post)
