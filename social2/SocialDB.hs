{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SocialDB
    ( SocialDB(..), User(..), Post(..), PostId(..)
    , emptySocialDB
    , feed, waitForFeed
    , newUser, getUser
    , newPost, getPost
    , follow, unfollow
    , like, unlike
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
    { users :: TVar (Map Text User)
    , posts :: TVar (Map PostId Post)
    }

data User = User
    { name      :: Text
    , timeline  :: TVar [Post]
    , following :: TVar (Set User)
    , followers :: TVar (Set User)
    }

data Post = Post
    { postId :: PostId
    , author :: User
    , time   :: UTCTime
    , body   :: Text
    , target :: User
    , likes  :: TVar (Set User)
    }

newtype PostId = PostId Word64
    deriving (Eq, Ord, Random, Show)

instance Eq User where
    u1 == u2 = name u1 == name u2

instance Ord User where
    compare = comparing name

instance Eq Post where
    p1 == p2 = postId p1 == postId p2

emptySocialDB :: IO SocialDB
emptySocialDB = do
    users <- newTVarIO Map.empty
    posts <- newTVarIO Map.empty
    return SocialDB{..}

------------------------------------------------------------------------------

instance Database SocialDB where
    data Operation SocialDB = NewUser Text
                            | NewPost PostId Text UTCTime Text Text
                            | Follow Text Text
                            | Unfollow Text Text
                            | Like Text PostId
                            | Unlike Text PostId

    replay (NewUser name) = void $ newUser name

    replay (NewPost postId authorName time body targetName) = do
        author <- getUser authorName
        target <- getUser targetName
        void $ newPost_ postId author time body target

    replay (Follow name1 name2) = do
        user1 <- getUser name1
        user2 <- getUser name2
        user1 `follow` user2

    replay (Unfollow name1 name2) = do
        user1 <- getUser name1
        user2 <- getUser name2
        user1 `unfollow` user2

    replay (Like name postId) = do
        user <- getUser name
        post <- getPost postId
        user `like` post

    replay (Unlike name postId) = do
        user <- getUser name
        post <- getPost postId
        user `unlike` post

------------------------------------------------------------------------------

feed :: User -> STM [Post]
feed user = do
    myPosts <- readTVar (timeline user)
    others <- Set.toList <$> readTVar (following user)
    otherPosts <- concat <$> mapM (readTVar . timeline) others
    return $ nub $ sortBy (flip $ comparing time) (myPosts ++ otherPosts)

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
        timeline <- newTVar []
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

newPost :: User -> Text -> User -> TX SocialDB Post
newPost author body target = do
    postId <- unsafeIOToTX randomIO
    db <- getData
    liftSTM $ do
        posts <- readTVar (posts db)
        check (Map.notMember postId posts)
    time <- unsafeIOToTX getCurrentTime
    newPost_ postId author time body target

newPost_ :: PostId -> User -> UTCTime -> Text -> User -> TX SocialDB Post
newPost_ postId author time body target = do
    record $ NewPost postId (name author) time body (name target)
    db <- getData
    liftSTM $ do
        likes <- newTVar Set.empty
        let post = Post{..}
        modifyTVar (timeline author) (post:)
        unless (target == author) $ modifyTVar (timeline target) (post:)
        modifyTVar (posts db) (Map.insert postId post)
        return post

getPost :: PostId -> TX SocialDB Post
getPost postId = do
    db <- getData
    postmap <- liftSTM $ readTVar (posts db)
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

like :: User -> Post -> TX SocialDB ()
like user post = do
    record $ Like (name user) (postId post)
    liftSTM $ modifyTVar (likes post) (Set.insert user)

unlike :: User -> Post -> TX SocialDB ()
unlike user post = do
    record $ Unlike (name user) (postId post)
    liftSTM $ modifyTVar (likes post) (Set.delete user)

------------------------------------------------------------------------------

deriveSafeCopy 1 'base ''PostId
deriveSafeCopyIndexedType 2 'extension ''Operation [''SocialDB]

------------------------------------------------------------------------------

data Operation_SocialDB_v0 = NewUser_v0 Text
                           | NewPost_v0 PostId Text UTCTime Text
                           | Follow_v0 Text Text
                           | Unfollow_v0 Text Text

instance Migrate (Operation SocialDB) where
    type MigrateFrom (Operation SocialDB) = Operation_SocialDB_v0
    migrate (NewUser_v0 name) = NewUser name
    migrate (NewPost_v0 postId authorName time body) =
        NewPost postId authorName time body authorName
    migrate (Follow_v0 name1 name2) = Follow name1 name2
    migrate (Unfollow_v0 name1 name2) = Unfollow name2 name2

deriveSafeCopy 1 'base ''Operation_SocialDB_v0
