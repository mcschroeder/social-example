{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SocialDB
    ( SocialDB(..), User(..), Post(..), PostId(..)
    , emptySocialDB
    , feed, waitForFeed
    , createUser, getUser
    , createPost
    , follow, unfollow
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.Map (Map)
import qualified Control.Concurrent.STM.Map as Map
import Control.Exception
import Control.Monad
import Data.Hashable
import Data.List
import Data.Ord
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time
import Data.Typeable
import Data.Word
import GHC.Conc.Sync (unsafeIOToSTM)
import System.Random

import TX

------------------------------------------------------------------------------

data SocialDB = SocialDB
    { users :: Map UserName User
    , posts :: Map PostId Post
    }

type UserName = Text

data User = User
    { name      :: UserName
    , timeline  :: TVar [Post]
    , following :: TVar (Set User)
    , followers :: TVar (Set User)
    }

newtype PostId = PostId Word64
    deriving (Eq, Ord, Random, Show, Hashable)

data Post = Post
    { postId :: PostId
    , author :: User
    , time   :: UTCTime
    , body   :: Text
    }

instance Eq User where
    u1 == u2 = name u1 == name u2

instance Ord User where
    compare = comparing name

emptySocialDB :: IO SocialDB
emptySocialDB = do
    users <- atomically $ Map.empty
    posts <- atomically $ Map.empty
    return SocialDB{..}

------------------------------------------------------------------------------

instance Database SocialDB where
    data Operation SocialDB = NewUser UserName
                            | NewPost PostId UserName UTCTime Text
                            | Follow UserName UserName
                            | Unfollow UserName UserName

    replay (NewUser name) = void $ createUser name

    replay (NewPost postId name time body) = do
        db <- getData
        author <- getUser name
        liftSTM $ void $ newPost postId author time body db

    replay (Follow name1 name2) = do
        user1 <- getUser name1
        user2 <- getUser name2
        user1 `follow` user2

    replay (Unfollow name1 name2) = do
        user1 <- getUser name1
        user2 <- getUser name2
        user1 `unfollow` user2

------------------------------------------------------------------------------

data SocialException = UserNotFound UserName
                     | UserAlreadyExists UserName
                     deriving (Show, Typeable)

instance Exception SocialException

------------------------------------------------------------------------------

feed :: User -> STM [Post]
feed user = do
    myPosts <- readTVar (timeline user)
    others <- Set.toList <$> readTVar (following user)
    otherPosts <- concat <$> mapM (readTVar . timeline) others
    return $ sortBy (flip $ comparing time) (myPosts ++ otherPosts)

waitForFeed :: User -> UTCTime -> STM [Post]
waitForFeed user lastSeen = do
    posts <- takeWhile isNew <$> feed user
    if null posts then retry else return posts
  where
    isNew post = diffUTCTime (time post) lastSeen > 0.1

createUser :: UserName -> TX SocialDB User
createUser name = do
    db <- getData
    record $ NewUser name
    liftSTM $ do
        alreadyExists <- Map.member name (users db)
        when alreadyExists (throwSTM $ UserAlreadyExists name)
        timeline <- newTVar []
        followers <- newTVar Set.empty
        following <- newTVar Set.empty
        let user = User{..}
        Map.insert name user (users db)
        return user

getUser :: UserName -> TX SocialDB User
getUser name = do
    db <- getData
    liftSTM $ do
        user <- Map.lookup name (users db)
        case user of
            Just user -> return user
            Nothing   -> throwSTM (UserNotFound name)

createPost :: User -> Text -> TX SocialDB Post
createPost author body = do
    db <- getData
    postId <- liftSTM $ newUniquePostId db
    time <- unsafeIOToTX getCurrentTime
    record $ NewPost postId (name author) time body
    liftSTM $ newPost postId author time body db

newUniquePostId :: SocialDB -> STM PostId
newUniquePostId db = do
    postId <- unsafeIOToSTM randomIO
    alreadyExists <- Map.member postId (posts db)
    check (not alreadyExists)
    return postId

newPost :: PostId -> User -> UTCTime -> Text -> SocialDB -> STM Post
newPost postId author time body db = do
    let post = Post{..}
    modifyTVar (timeline author) (post:)
    Map.insert postId post (posts db)
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

deriveSafeCopy 1 'base ''PostId
deriveSafeCopyIndexedType 1 'base ''Operation [''SocialDB]
