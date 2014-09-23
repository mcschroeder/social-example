{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text
import Data.Time
import Data.Word

------------------------------------------------------------------------------

data SocialDB = SocialDB
    { users :: TVar (Map UserId User) }

newtype UserId = UserId Word64 deriving (Eq, Ord, Show, Read)

data User = User
    { userId :: UserId
    , name :: TVar Text
    , friends :: TVar (Set User)
    , posts :: TVar (Map UTCTime Post)
    , activityLog :: TVar [(Activity, UTCTime)]
    }

data Activity = Posted Post
              | Liked Post
              | Friended User

data Post = Post
    { author :: User
    , creationTime :: UTCTime
    , body :: TVar Text
    , likedBy :: TVar (Set User)
    }

instance Eq User where
    (==) = (==) `on` userId

instance Ord User where
    compare = comparing userId

instance Eq Post where
    a == b = compare a b == EQ

instance Ord Post where
    compare = mconcat [comparing author, comparing creationTime]

------------------------------------------------------------------------------

instance Durable SocialDB where
    data Operation SocialDB = AddUser UserId Text
                            | AddPost UserId UTCTime Text
                            | Like UserId UserId UTCTime
                            | Unlike UserId UserId UTCTime
                            | Friend UserId UserId
                            | Unfriend UserId UserId
                            deriving (Show, Read)


    replay (AddUser userId name) =
        void $ addUser userId name

    replay (AddPost author creationTime body) =
        void $ addPost author creationTime body

    replay (Like uid1 uid2 t) = do
        user <- getUser uid1
        author <- getUser uid2
        post <- getPost author t
        like user post

    replay (Unlike uid1 uid2 t) = do
        user <- getUser uid1
        author <- getUser uid2
        post <- getPost author t
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

getUser :: UserId -> TX SocialDB User
getUser uid = do
    db <- getDatabase
    usermap <- liftSTM $ readTVar (users db)
    case Map.lookup uid usermap of
        Just user -> return user
        Nothing -> error $ "user not found: " ++ (show uid)  -- TODO

newUser :: Text -> TX SocialDB User
newUser name = do
    userId <- UserId <$> randomId
    db <- getDatabase
    liftSTM $ do
        usermap <- readTVar (users db)
        check (Map.notMember userId usermap)
    addUser userId name

addUser :: UserId -> Text -> TX SocialDB User
addUser userId name = do
    db <- getDatabase
    usermap <- liftSTM $ readTVar (users db)
    unless (Map.notMember userId usermap)
           (error $ "user already exists: " ++ show userId)  -- TODO
    user <- liftSTM $ do
        name <- newTVar name
        friends <- newTVar Set.empty
        posts <- newTVar Map.empty
        activityLog <- newTVar []
        return User{..}
    liftSTM $ modifyTVar' (users db) (Map.insert userId user)
    record $ AddUser userId name
    return user

getPost :: User -> UTCTime -> TX SocialDB Post
getPost user creationTime = do
    postmap <- liftSTM $ readTVar (posts user)
    case Map.lookup creationTime postmap of
        Just post -> return post
        Nothing -> error $ "post not found: " ++ (show creationTime)  -- TODO

newPost :: User -> Text -> TX SocialDB Post
newPost author body = do
    creationTime <- getTime
    liftSTM $ do
        postmap <- readTVar (posts author)
        check (Map.notMember creationTime postmap)
    addPost (userId author) creationTime body

addPost :: UserId -> UTCTime -> Text -> TX SocialDB Post
addPost authorId creationTime body = do
    author <- getUser authorId
    postmap <- liftSTM $ readTVar (posts author)
    unless (Map.notMember creationTime postmap)
           (error $ "post already exists: " ++ show creationTime)  -- TODO
    post <- liftSTM $ do
        body <- newTVar body
        likedBy <- newTVar Set.empty
        return Post{..}
    liftSTM $ modifyTVar' (posts author) (Map.insert creationTime post)
    record $ AddPost authorId creationTime body
    return post

like :: User -> Post -> TX SocialDB ()
like user post = do
    now <- getTime
    liftSTM $ do
        modifyTVar' (likedBy post) (Set.insert user)
        modifyTVar' (activityLog user) ((Liked post, now):)
    record $ Like (userId user) (userId . author $ post) (creationTime post)

unlike :: User -> Post -> TX SocialDB ()
unlike user post = do
    liftSTM $ modifyTVar' (likedBy post) (Set.delete user)
    record $ Unlike (userId user) (userId . author $ post) (creationTime post)

friend :: User -> User -> TX SocialDB ()
friend user1 user2 = do
    now <- getTime
    liftSTM $ do
        modifyTVar' (friends user1) (Set.insert user2)
        modifyTVar' (friends user2) (Set.insert user1)
        modifyTVar' (activityLog user1) ((Friended user2, now):)
        modifyTVar' (activityLog user2) ((Friended user1, now):)
    record $ Friend (userId user1) (userId user2)


unfriend :: User -> User -> TX SocialDB ()
unfriend user1 user2 = do
    liftSTM $ do
        modifyTVar' (friends user1) (Set.delete user2)
        modifyTVar' (friends user2) (Set.delete user1)
    record $ Unfriend (userId user1) (userId user2)

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


