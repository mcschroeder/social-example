{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module SocialDB
    ( SocialDB(..), User(..), Post(..)
    , emptySocialDB
    , newUser, newPost
    , getUser, getPost
    , like, unlike, becomeFriends
    ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time
import Data.Typeable

import TX

------------------------------------------------------------------------------

data SocialDB = SocialDB
    { users :: TVar (Map Text User) }

data User = User
    { name    :: Text
    , friends :: TVar (Set User)
    , posts   :: TVar (Map UTCTime Post)
    }

data Post = Post
    { author  :: User
    , time    :: UTCTime
    , body    :: Text
    , likedBy :: TVar (Set User)
    }

instance Eq User where
    (==) = (==) `on` name

instance Ord User where
    compare = comparing name

instance Show User where
    show = show . name

instance Eq Post where
    a == b = compare a b == EQ

instance Ord Post where
    compare = mconcat [comparing author, comparing time]

------------------------------------------------------------------------------

emptySocialDB :: IO (SocialDB)
emptySocialDB = do
    users <- newTVarIO Map.empty
    return SocialDB{..}

instance Durable SocialDB where
    data Operation SocialDB = NewUser Text
                            | AddPost Text UTCTime Text
                            | Like Text Text UTCTime
                            | Unlike Text Text UTCTime
                            | BecomeFriends Text Text
                            deriving (Show, Read)

    encode = C.pack . show
    decode = read . C.unpack

    replay (NewUser name) = void $ newUser name

    replay (AddPost name time body) = do user <- getUser name
                                         void $ addPost user time body

    replay (Like name1 name2 time) = do user <- getUser name1
                                        author <- getUser name2
                                        post <- getPost author time
                                        like user post

    replay (Unlike name1 name2 time) = do user <- getUser name1
                                          author <- getUser name2
                                          post <- getPost author time
                                          unlike user post

    replay (BecomeFriends name1 name2) = do user1 <- getUser name1
                                            user2 <- getUser name2
                                            becomeFriends user1 user2

data SocialException = UserAlreadyExists Text
                     | UserNotFound Text
                     | PostNotFound User UTCTime
                     deriving (Typeable, Show)

instance Exception SocialException


newUser :: Text -> TX SocialDB User
newUser name = do
    db <- getData
    record $ NewUser name
    liftSTM $ do
        usermap <- readTVar (users db)
        when (Map.member name usermap) (throwSTM $ UserAlreadyExists name)
        friends <- newTVar Set.empty
        posts <- newTVar Map.empty
        let user = User{..}
        modifyTVar (users db) (Map.insert name user)
        return user

newPost :: User -> Text -> TX SocialDB Post
newPost author body = do
    time <- unsafeIOToTX getCurrentTime
    addPost author time body

addPost :: User -> UTCTime -> Text -> TX SocialDB Post
addPost author time body = do
    record $ AddPost (name author) time body
    liftSTM $ do
        likedBy <- newTVar Set.empty
        let post = Post{..}
        modifyTVar (posts author) (Map.insert time post)
        return post

getUser :: Text -> TX SocialDB User
getUser name = do
    db <- getData
    liftSTM $ do
        usermap <- readTVar (users db)
        case Map.lookup name usermap of
            Just user -> return user
            Nothing -> throwSTM $ UserNotFound name

getPost :: User -> UTCTime -> TX SocialDB Post
getPost author time = liftSTM $ do
    postmap <- readTVar (posts author)
    case Map.lookup time postmap of
        Just post -> return post
        Nothing -> throwSTM $ PostNotFound author time

like :: User -> Post -> TX SocialDB ()
like user post = do
    record $ Like (name user) (name $ author post) (time post)
    liftSTM $ modifyTVar (likedBy post) (Set.insert user)

unlike :: User -> Post -> TX SocialDB ()
unlike user post = do
    record $ Unlike (name user) (name $ author post) (time post)
    liftSTM $ modifyTVar (likedBy post) (Set.delete user)

becomeFriends :: User -> User -> TX SocialDB ()
becomeFriends user1 user2 = do
    record $ BecomeFriends (name user1) (name user2)
    liftSTM $ do
        modifyTVar (friends user1) (Set.insert user2)
        modifyTVar (friends user2) (Set.insert user1)

