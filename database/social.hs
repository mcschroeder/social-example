{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text
import Data.Time
import Data.Word

------------------------------------------------------------------------------

data SocialDB = SocialDB
    { users :: TVar (Map UserId UserVar) }

data User = User
    { userId :: UserId
    , name :: Text
    , friends :: TVar [UserVar]
    , posts :: TVar (Map PostId PostVar)
    , activityLog :: TVar [(Activity, UTCTime)]
    }

data Activity = Posted PostVar
              | Liked PostVar
              | Friended UserVar

data Post = Post
    { postId :: PostId  -- only unique per author
    , author :: UserVar
    , creationTime :: UTCTime
    , body :: Text
    , likedBy :: TVar [UserVar]
    }

newtype UserId = UserId Word64 deriving (Eq, Ord, Show, Read)
newtype PostId = PostId Word64 deriving (Eq, Ord, Show, Read)

type UserVar = TVar User
type PostVar = TVar Post

------------------------------------------------------------------------------

instance Durable SocialDB where
    data Operation SocialDB = AddUser UserId Text
                            | AddPost PostId UserId UTCTime Text
                            | Like UserId UserId PostId
                            | Unlike UserId UserId PostId
                            | Friend UserId UserId
                            | Unfriend UserId UserId
                            deriving (Show, Read)


    replay (AddUser userId name) =
        void $ addUser userId name

    replay (AddPost postId author creationTime body) =
        void $ addPost postId author creationTime body

    replay (Like uid1 uid2 pid) = do
        user <- getUser uid1
        author <- getUser uid2
        post <- getPost author pid
        like user post

    replay (Unlike uid1 uid2 pid) = do
        user <- getUser uid1
        author <- getUser uid2
        post <- getPost author pid
        unlike user post

    replay (Friend uid1 uid2) = do
        u1 <- getUser uid1
        u2 <- getUser uid2
        friend u1 u2

    replay (Unfriend uid1 uid2) = do
        u1 <- getUser uid1
        u2 <- getUser uid2
        unfriend u1 u2

randomId :: TX SocialDB Word64
randomId = undefined

getUser :: UserId -> TX SocialDB UserVar
getUser uid = do
    db <- getDatabase
    usermap <- liftSTM $ readTVar (users db)
    case Map.lookup uid usermap of
        Just uvar -> return uvar
        Nothing -> error $ "user not found: " ++ (show uid)  -- TODO

newUser :: Text -> TX SocialDB UserVar
newUser name = do
    userId <- UserId <$> randomId
    db <- getDatabase
    liftSTM $ do
        usermap <- readTVar (users db)
        check (Map.notMember userId usermap)
    addUser userId name

addUser :: UserId -> Text -> TX SocialDB UserVar
addUser userId name = do
    db <- getDatabase
    usermap <- liftSTM $ readTVar (users db)
    unless (Map.notMember userId usermap)
           (error $ "user already exists: " ++ show userId)  -- TODO
    uvar <- liftSTM $ do
        friends <- newTVar []
        posts <- newTVar Map.empty
        activityLog <- newTVar []
        newTVar User{..}
    liftSTM $ modifyTVar' (users db) (Map.insert userId uvar)
    record $ AddUser userId name
    return uvar

getPost :: UserVar -> PostId -> TX SocialDB PostVar
getPost uvar pid = do
    u <- liftSTM $ readTVar uvar
    postmap <- liftSTM $ readTVar (posts u)
    case Map.lookup pid postmap of
        Just pvar -> return pvar
        Nothing -> error $ "post not found: " ++ (show pid)  -- TODO

newPost :: UserVar -> Text -> TX SocialDB PostVar
newPost author body = do
    postId <- PostId <$> randomId
    user <- liftSTM $ readTVar author
    liftSTM $ do
        postmap <- readTVar (posts user)
        check (Map.notMember postId postmap)
    creationTime <- getTime
    addPost postId (userId user) creationTime body

addPost :: PostId -> UserId -> UTCTime -> Text -> TX SocialDB PostVar
addPost postId authorId creationTime body = do
    author <- getUser authorId
    postmap <- liftSTM $ readTVar <$> posts =<< readTVar author
    unless (Map.notMember postId postmap)
           (error $ "post already exists: " ++ show postId)  -- TODO
    pvar <- liftSTM $ do
        likedBy <- newTVar []
        newTVar Post{..}
    liftSTM $ do
        postmapVar <- posts <$> readTVar author
        modifyTVar' postmapVar (Map.insert postId pvar)
    record $ AddPost postId authorId creationTime body
    return pvar

like :: UserVar -> PostVar -> TX SocialDB ()
like uvar pvar = do
    now <- getTime
    user <- liftSTM $ readTVar uvar
    post <- liftSTM $ readTVar pvar
    authorId <- liftSTM $ userId <$> readTVar (author post)
    liftSTM $ do
        modifyTVar' (likedBy post) (uvar:)
        modifyTVar' (activityLog user) ((Liked pvar, now):)
    record $ Like (userId user) authorId (postId post)

unlike :: UserVar -> PostVar -> TX SocialDB ()
unlike uvar pvar = do
    user <- liftSTM $ readTVar uvar
    post <- liftSTM $ readTVar pvar
    authorId <- liftSTM $ userId <$> readTVar (author post)
    liftSTM $ modifyTVar' (likedBy post) (delete uvar)
    record $ Unlike (userId user) authorId (postId post)

friend :: UserVar -> UserVar -> TX SocialDB ()
friend v1 v2 = do
    now <- getTime
    u1 <- liftSTM $ readTVar v1
    u2 <- liftSTM $ readTVar v2
    liftSTM $ do
        modifyTVar' (friends u1) (v2:)
        modifyTVar' (friends u2) (v1:)
        modifyTVar' (activityLog u1) ((Friended v2, now):)
        modifyTVar' (activityLog u2) ((Friended v1, now):)
    record $ Friend (userId u1) (userId u2)


unfriend :: UserVar -> UserVar -> TX SocialDB ()
unfriend v1 v2 = do
    u1 <- liftSTM $ readTVar v1
    u2 <- liftSTM $ readTVar v2
    liftSTM $ do
        modifyTVar' (friends u1) (delete v2)
        modifyTVar' (friends u2) (delete v1)
    record $ Unfriend (userId u1) (userId u2)

------------------------------------------------------------------------------

newtype TX d a = TX (STM a)
  deriving (Functor, Applicative, Monad)

type Serializable a = (Show a, Read a)

class Serializable (Operation d) => Durable d where
    data Operation d
    replay :: Operation d -> TX d ()

record :: Durable d => Operation d -> TX d ()
record = undefined

liftSTM :: STM a -> TX d a
liftSTM = undefined

getTime :: TX d UTCTime
getTime = undefined

getDatabase :: TX d d
getDatabase = undefined


