
data BlogDB = BlogDB
    { users :: TVar (Map Text User)
    , allPosts :: TVar (Map PostId Post)
    }

data User = User
    { name :: Text
    , posts :: TVar [Post]
    , following :: TVar (Set User)
    , followers :: TVar (Set User)
    }

data Post = Post
    { author :: User
    , time :: UTCTime
    , body :: Content
    , reblogs :: TVar [Post]
    }

data Content = Original Text
             | Reblogged Post

data PostId = PostId Word64

feed :: User -> STM [Post]
feed user = do
    myPosts <- readTVar (posts user)
    others <- Set.toList <$> readTVar (following user)
    otherPosts <- concat <$> mapM (readTVar . posts) others
    return $ sortBy (flip $ comparing time) (myPosts ++ otherPosts)

waitForFeed :: User -> UTCTime -> STM [Post]
waitForFeed user lastSeen = do
    posts <- feed user
    if time (head posts) > lastSeen
        then return posts
        else retry

newUser :: Text -> TX BlogDB User
newUser name = do
    db <- getData
    record $ NewUser name
    liftSTM $ do
        posts <- newTVar []
        followers <- newTVar Set.empty
        let user = User{..}
        modifyTVar (users db) (Map.insert name user)

getUser :: Text -> TX BlogDB User
getUser name = do
    db <- getData
    usermap <- readTVar (users db)
    case Map.lookup name usermap of
        Just user -> return user
        Nothing -> error $ "user not found: " ++ name

uniquePostId :: TX BlogDB PostId
uniquePostId = do
    db <- getData
    postId <- PostId <$> unsafeIOToTX randomIO
    liftSTM $ do
        postmap <- readTVar (allPosts db)
        check (not $ Map.member postId postmap)
    return postId

newPost :: User -> Text -> TX BlogDB Post
newPost author body = do
    postId <- uniquePostId
    time <- unsafeIOToTX getTime
    newPost_ postId time author body

--replay (NewPost postId time name body) = do
--    author <- getUser name
--    addPost_ postId time author body

newPost_ :: PostId -> UTCTime -> User -> Text -> TX BlogDB Post
newPost_ postId time author body = do
    record $ NewPost postId time (name author) body
    liftSTM $ addPost postId time author (Original body)

addPost :: PostId -> UTCTime -> User -> Content -> TX BlogDB Post
addPost postId time author body = do
    db <- getData
    liftSTM $ do
        reblogs <- newTVar []
        let post = Post{..}
        modifyTVar (allPosts db) (Map.insert postId post)
        modifyTVar (posts $ author post) (post:)
        return post

reblog :: User -> Post -> TX BlogDB Post
reblog author source = do
    postId <- uniquePostId
    time <- unsafeIOToTX getCurrentTime
    reblog_ postId time author source

--replay (Reblog postId time name sourceId) = do
--    author <- getUser name
--    source <- getPost sourceId
--    reblog_ postId time author source

reblog_ :: PostId -> UTCTime -> User -> Post -> TX BlogDB Post
reblog_ postId time author source = do
    record $ Reblog postId time (name author) (postId source)
    liftSTM $ do
        post <- addPost postId time author (Reblogged source)
        modifyTVar (reblogs source) (post:)
        return post

--replay (Follow name1 name2) = do
--    user1 <- getUser name1
--    user2 <- getUser name2
--    follow user1 user2

follow :: User -> User -> TX BlogDB ()
follow user1 user2 = do
    record $ Follow (name user1) (name user2)
    liftSTM $ do
        modifyTVar (following user1) (Set.insert user2)
        modifyTVar (followers user2) (Set.insert user1)
