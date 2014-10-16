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
    , newPost, getPost, reblog
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
    deriving (Eq, Ord, Random, Show)

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
                            | Reblog PostId Text UTCTime PostId
                            | Follow Text Text
                            | Unfollow Text Text

    replay (NewUser name) = void $ newUser name

    replay (NewPost postId name time body) = do
        author <- getUser name
        void $ newPost_ postId author time body

    replay (Reblog postId name time postId2) = do
        author <- getUser name
        post2 <- getPost postId2
        void $ reblog_ postId author time post2

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
    createPost postId author time (Original text)

reblog :: User -> Post -> TX SocialDB Post
reblog author originalPost = do
    time <- unsafeIOToTX getCurrentTime
    postId <- unsafeIOToTX randomIO
    reblog_ postId author time originalPost

reblog_ :: PostId -> User -> UTCTime -> Post -> TX SocialDB Post
reblog_ pid author time originalPost = do
    db <- getData
    record $ Reblog pid (name author) time (postId originalPost)
    post <- createPost pid author time (Reblogged originalPost)
    liftSTM $ modifyTVar (reblogs originalPost) (post:)
    return post

createPost :: PostId -> User -> UTCTime -> Content -> TX SocialDB Post
createPost postId author time body = do
    db <- getData
    reblogs <- liftSTM $ newTVar []
    let post = Post{..}
    liftSTM $ do
        modifyTVar (posts author) (post:)
        modifyTVar (allPosts db) (Map.insert postId post)
    return post

getPost :: PostId -> TX SocialDB Post
getPost postId = do
    db <- getData
    postmap <- liftSTM $ readTVar (allPosts db)
    case Map.lookup postId postmap of
        Just post -> return post
        Nothing -> error $ "post not found: " ++ show postId

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

deriveSafeCopyIndexedType 2 'extension ''Operation [''SocialDB]
deriveSafeCopy 1 'base ''PostId

------------------------------------------------------------------------------

-- TODO: we need to be able to migrate within a TX environment
-- so that we can create new unique ids for posts

data Operation_SocialDB_v0 = NewUser_v0 Text
                           | NewPost_v0 Text UTCTime Text
                           | Follow_v0 Text Text
                           | Unfollow_v0 Text Text

deriveSafeCopy 1 'base ''Operation_SocialDB_v0

instance Migrate (Operation SocialDB) where
    type MigrateFrom (Operation SocialDB) = Operation_SocialDB_v0
    migrate (NewUser_v0 name) = NewUser name
    migrate (NewPost_v0 name time body) = NewPost (PostId 0) name time body
    migrate (Follow_v0 name1 name2) = Follow name1 name2
    migrate (Unfollow_v0 name1 name2) = Unfollow name1 name2
