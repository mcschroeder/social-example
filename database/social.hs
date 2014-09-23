{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text
import Data.Time
import Data.Word

------------------------------------------------------------------------------

data SocialDB = SocialDB
    { users :: TVar (Map UserId (TVar User)) }

data User = User
    { userId :: UserId
    , name :: Text
    , friends :: TVar (Set UserId)
    , posts :: TVar (Map PostId (TVar Post))
    , activityLog :: TVar [(Activity, UTCTime)]
    }

data Activity = Posted PostId
              | Liked UserId PostId
              | Friended UserId

data Post = Post
    { postId :: PostId  -- only unique per author
    , author :: UserId
    , creationTime :: UTCTime
    , body :: Text
    , likedBy :: TVar (Set UserId)
    }

newtype UserId = UserId Word64 deriving (Show, Eq, Ord)
newtype PostId = PostId Word64 deriving (Show, Eq, Ord)

------------------------------------------------------------------------------

instance Durable SocialDB where
    data Operation SocialDB = AddUser User
                            | AddPost Post
                            | Like UserId UserId PostId
                            | Unlike UserId UserId PostId
                            | Friend UserId UserId
                            | Unfriend UserId UserId

    replay (AddUser user) = void $ addUser user
    replay (AddPost post) = void $ addPost post
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

getUser :: UserId -> TX SocialDB (TVar User)
getUser uid = do
    db <- getDatabase
    usermap <- liftSTM $ readTVar (users db)
    case Map.lookup uid usermap of
        Just uvar -> return uvar
        Nothing -> error $ "user not found: " ++ (show uid)  -- TODO

newUser :: Text -> TX SocialDB (TVar User)
newUser name = do
    userId <- UserId <$> randomId
    db <- getDatabase
    liftSTM $ do
        usermap <- readTVar (users db)
        check (Map.notMember userId usermap)
    user <- liftSTM $ do
        friends <- newTVar Set.empty
        posts <- newTVar Map.empty
        activityLog <- newTVar []
        return $ User { .. }
    addUser user

addUser :: User -> TX SocialDB (TVar User)
addUser user@User{..} = do
    db <- getDatabase
    usermap <- liftSTM $ readTVar (users db)
    unless (Map.notMember userId usermap)
           (error $ "user already exists: " ++ show userId)  -- TODO
    uvar <- liftSTM $ newTVar user
    liftSTM $ modifyTVar' (users db) (Map.insert userId uvar)
    record $ AddUser user
    return uvar

getPost :: TVar User -> PostId -> TX SocialDB (TVar Post)
getPost uvar pid = do
    u <- liftSTM $ readTVar uvar
    postmap <- liftSTM $ readTVar (posts u)
    case Map.lookup pid postmap of
        Just pvar -> return pvar
        Nothing -> error $ "post not found: " ++ (show pid)  -- TODO

newPost :: TVar User -> Text -> TX SocialDB (TVar Post)
newPost uvar body = do
    u <- liftSTM $ readTVar uvar
    let author = userId u
    postId <- PostId <$> randomId
    liftSTM $ do
        postmap <- readTVar (posts u)
        check (Map.notMember postId postmap)
    creationTime <- getTime
    likedBy <- liftSTM $ newTVar Set.empty
    addPost Post{..}

addPost :: Post -> TX SocialDB (TVar Post)
addPost post@Post{..} = do
    uvar <- getUser author
    u <- liftSTM $ readTVar uvar
    postmap <- liftSTM $ readTVar (posts u)
    unless (Map.notMember postId postmap)
           (error $ "post already exists: " ++ show postId)  -- TODO
    pvar <- liftSTM $ newTVar post
    liftSTM $ modifyTVar' (posts u) (Map.insert postId pvar)
    record $ AddPost post
    return pvar

like :: TVar User -> TVar Post -> TX SocialDB ()
like uvar pvar = do
    now <- getTime
    User{..} <- liftSTM $ readTVar uvar
    Post{..} <- liftSTM $ readTVar pvar
    liftSTM $ do
        modifyTVar' likedBy (Set.insert userId)
        modifyTVar' activityLog ((Liked author postId, now):)
    record $ Like userId author postId

unlike :: TVar User -> TVar Post -> TX SocialDB ()
unlike uvar pvar = do
    User{..} <- liftSTM $ readTVar uvar
    Post{..} <- liftSTM $ readTVar pvar
    liftSTM $ modifyTVar' likedBy (Set.delete userId)
    record $ Unlike userId author postId

friend :: TVar User -> TVar User -> TX SocialDB ()
friend v1 v2 = do
    now <- getTime
    u1 <- liftSTM $ readTVar v1
    u2 <- liftSTM $ readTVar v2
    liftSTM $ do
        modifyTVar' (friends u1) (Set.insert (userId u2))
        modifyTVar' (friends u2) (Set.insert (userId u1))
        modifyTVar' (activityLog u1) ((Friended (userId u2), now):)
        modifyTVar' (activityLog u2) ((Friended (userId u1), now):)
    record $ Friend (userId u1) (userId u2)


unfriend :: TVar User -> TVar User -> TX SocialDB ()
unfriend v1 v2 = do
    u1 <- liftSTM $ readTVar v1
    u2 <- liftSTM $ readTVar v2
    liftSTM $ do
        modifyTVar' (friends u1) (Set.delete (userId u2))
        modifyTVar' (friends u2) (Set.delete (userId u1))
    record $ Unfriend (userId u1) (userId u2)

------------------------------------------------------------------------------

newtype TX d a = TX (STM a)
  deriving (Functor, Applicative, Monad)

class Durable d where
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


