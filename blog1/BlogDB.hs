{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module BlogDB where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time

import TX

------------------------------------------------------------------------------

data BlogDB = BlogDB
    { users :: TVar (Map Text User) }

data User = User
    { name :: Text
    , posts :: TVar [Post]
    , following :: TVar (Set User)
    , followers :: TVar (Set User)
    }

data Post = Post
    { author :: User
    , time :: UTCTime
    , body :: Text
    }

instance Eq User where
    u1 == u2 = name u1 == name u2

instance Ord User where
    compare = comparing name

emptyBlogDB :: IO BlogDB
emptyBlogDB = do
    users <- newTVarIO Map.empty
    return BlogDB{..}

------------------------------------------------------------------------------

instance Durable BlogDB where
    data Operation BlogDB = NewUser Text
                          | NewPost Text UTCTime Text
                          | Follow Text Text
                          | Unfollow Text Text
                          deriving (Show, Read)

    encode = C.pack . show
    decode = read . C.unpack

    replay (NewUser name) = void $ newUser name

    replay (NewPost name time body) = do
        author <- getUser name
        void $ newPost_ author time body

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

newUser :: Text -> TX BlogDB User
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

getUser :: Text -> TX BlogDB User
getUser name = do
    db <- getData
    usermap <- liftSTM $ readTVar (users db)
    case Map.lookup name usermap of
        Just user -> return user
        Nothing -> error $ "user not found: " ++ show name

newPost :: User -> Text -> TX BlogDB Post
newPost author body = do
    time <- unsafeIOToTX getCurrentTime
    newPost_ author time body

newPost_ :: User -> UTCTime -> Text -> TX BlogDB Post
newPost_ author time body = do
    record $ NewPost (name author) time body
    liftSTM $ do
        let post = Post{..}
        modifyTVar (posts author) (post:)
        return post

follow :: User -> User -> TX BlogDB ()
follow user1 user2 = do
    record $ Follow (name user1) (name user2)
    liftSTM $ do
        modifyTVar (following user1) (Set.insert user2)
        modifyTVar (followers user2) (Set.insert user1)

unfollow :: User -> User -> TX BlogDB ()
unfollow user1 user2 = do
    record $ Unfollow (name user1) (name user2)
    liftSTM $ do
        modifyTVar (following user1) (Set.delete user2)
        modifyTVar (followers user2) (Set.delete user1)
