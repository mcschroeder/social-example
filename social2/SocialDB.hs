{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SocialDB
    ( SocialDB(..), User(..), Post(..), Content(..), PostId(..)
    , emptySocialDB
    , feed, waitForFeed
    , newUser, getUser
    , newPost
    , follow, unfollow
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time
import Data.Word
import System.Random

import TX

------------------------------------------------------------------------------

data SocialDB = SocialDB
    { users    :: TVar (Map Text User)
    , allPosts :: TVar (Map PostId Post)
    }

data User = User
    { name      :: Text
    , posts     :: TVar [Post]
    , following :: TVar (Set User)
    , followers :: TVar (Set User)
    }

data Post = Post
    { postId  :: PostId
    , author  :: User
    , time    :: UTCTime
    , body    :: Content
    , reblogs :: TVar [Post]
    }

data Content = Original Text
             | Reblogged Post

newtype PostId = PostId Word64
    deriving (Eq, Ord, Random)

instance Eq User where
    u1 == u2 = name u1 == name u2

instance Ord User where
    compare = comparing name

emptySocialDB :: IO SocialDB
emptySocialDB = do
    users <- newTVarIO Map.empty
    allPosts <- newTVarIO Map.empty
    return SocialDB{..}

------------------------------------------------------------------------------

instance Durable SocialDB where
    data Operation SocialDB = NewUser Text
                            | NewPost PostId Text UTCTime Text
                            | Follow Text Text
                            | Unfollow Text Text

    replay (NewUser name) = void $ newUser name

    replay (NewPost postId name time body) = do
        author <- getUser name
        void $ newPost_ postId author time body

    replay (Follow name1 name2) = do
        user1 <- getUser name1
        user2 <- getUser name2
        follow user1 user2

    replay (Unfollow name1 name2) = do
        user1 <- getUser name1
        user2 <- getUser name2
        unfollow user1 user2

------------------------------------------------------------------------------

feed :: User -> STM [Post]
feed user = do
    myPosts <- readTVar (posts user)
    others <- Set.toList <$> readTVar (following user)
    otherPosts <- concat <$> mapM (readTVar . posts) others
    return $ sortBy (flip $ comparing time) (myPosts ++ otherPosts)

waitForFeed :: User -> UTCTime -> STM [Post]
waitForFeed user lastSeen = do
    posts <- takeWhile isNew <$> feed user
    if null posts then retry else return posts
  where
    isNew post = diffUTCTime (time post) lastSeen > 0.1

newUser :: Text -> TX SocialDB User
newUser name = do
    db <- getData
    record $ NewUser name
    liftSTM $ do
        usermap <- readTVar (users db)
        unless (Map.notMember name usermap)
               (error $ "user already exists: " ++ show name)
        posts <- newTVar []
        followers <- newTVar Set.empty
        following <- newTVar Set.empty
        let user = User{..}
        modifyTVar (users db) (Map.insert name user)
        return user

getUser :: Text -> TX SocialDB User
getUser name = do
    db <- getData
    usermap <- liftSTM $ readTVar (users db)
    case Map.lookup name usermap of
        Just user -> return user
        Nothing -> error $ "user not found: " ++ show name

newPost :: User -> Text -> TX SocialDB Post
newPost author body = do
    time <- unsafeIOToTX getCurrentTime
    postId <- unsafeIOToTX randomIO
    newPost_ postId author time body

newPost_ :: PostId -> User -> UTCTime -> Text -> TX SocialDB Post
newPost_ postId author time text = do
    db <- getData
    record $ NewPost postId (name author) time text
    liftSTM $ do
        let body = Original text
        reblogs <- newTVar []
        let post = Post{..}
        modifyTVar (posts author) (post:)
        modifyTVar (allPosts db) (Map.insert postId post)
        return post

follow :: User -> User -> TX SocialDB ()
follow user1 user2 = do
    record $ Follow (name user1) (name user2)
    liftSTM $ do
        modifyTVar (following user1) (Set.insert user2)
        modifyTVar (followers user2) (Set.insert user1)

unfollow :: User -> User -> TX SocialDB ()
unfollow user1 user2 = do
    record $ Unfollow (name user1) (name user2)
    liftSTM $ do
        modifyTVar (following user1) (Set.delete user2)
        modifyTVar (followers user2) (Set.delete user1)

------------------------------------------------------------------------------

deriveSafeCopyIndexedType 1 'base ''Operation [''SocialDB]
deriveSafeCopy 1 'base ''PostId
