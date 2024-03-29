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
    , createPost, getPost
    , follow, unfollow
    , like, unlike
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
    , target :: User
    , likes  :: TVar (Set User)
    }

instance Eq User where
    u1 == u2 = name u1 == name u2

instance Ord User where
    compare = comparing name

instance Eq Post where
    p1 == p2 = postId p1 == postId p2

emptySocialDB :: IO SocialDB
emptySocialDB = do
    users <- atomically $ Map.empty
    posts <- atomically $ Map.empty
    return SocialDB{..}

------------------------------------------------------------------------------

instance Database SocialDB where
    data Operation SocialDB = NewUser UserName
                            | NewPost PostId UserName UTCTime Text UserName
                            | Follow UserName UserName
                            | Unfollow UserName UserName
                            | Like UserName PostId
                            | Unlike UserName PostId

    replay (NewUser name) = void $ createUser name

    replay (NewPost postId authorName time body targetName) = do
        db <- getData
        author <- getUser authorName
        target <- getUser targetName
        liftSTM $ void $ newPost postId author time body target db

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

data SocialException = UserNotFound UserName
                     | UserAlreadyExists UserName
                     | PostNotFound PostId
                     deriving (Show, Typeable)

instance Exception SocialException

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

createPost :: User -> Text -> User -> TX SocialDB Post
createPost author body target = do
    db <- getData
    postId <- liftSTM $ newUniquePostId db
    time <- unsafeIOToTX getCurrentTime
    record $ NewPost postId (name author) time body (name target)
    liftSTM $ newPost postId author time body target db

newUniquePostId :: SocialDB -> STM PostId
newUniquePostId db = do
    postId <- unsafeIOToSTM randomIO
    alreadyExists <- Map.member postId (posts db)
    check (not alreadyExists)
    return postId

newPost :: PostId -> User -> UTCTime -> Text -> User -> SocialDB -> STM Post
newPost postId author time body target db = do
    likes <- newTVar Set.empty
    let post = Post{..}
    modifyTVar (timeline author) (post:)
    unless (target == author) $ modifyTVar (timeline target) (post:)
    Map.insert postId post (posts db)
    return post

getPost :: PostId -> TX SocialDB Post
getPost postId = do
    db <- getData
    liftSTM $ do
        post <- Map.lookup postId (posts db)
        case post of
            Just post -> return post
            Nothing   -> throwSTM (PostNotFound postId)

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

data Operation_SocialDB_v0 = NewUser_v0 UserName
                           | NewPost_v0 PostId UserName UTCTime Text
                           | Follow_v0 UserName UserName
                           | Unfollow_v0 UserName UserName

instance Migrate (Operation SocialDB) where
    type MigrateFrom (Operation SocialDB) = Operation_SocialDB_v0
    migrate (NewUser_v0 name) = NewUser name
    migrate (NewPost_v0 postId authorName time body) =
        NewPost postId authorName time body authorName
    migrate (Follow_v0 name1 name2) = Follow name1 name2
    migrate (Unfollow_v0 name1 name2) = Unfollow name2 name2

deriveSafeCopy 1 'base ''Operation_SocialDB_v0
