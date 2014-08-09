{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO
import System.IO.Error

------------------------------------------------------------------------------

main :: IO ()
main = do
    (fp:cmd:args) <- getArgs

    empty <- List <$> newTVarIO []
    db <- openDatabase empty fp

    case cmd of
        "add" -> do let str = args !! 0
                    runTransaction db $ addItem str

        "update" -> do let i = read $ args !! 0
                           str = args !! 1
                       runTransaction db $ updateItem i str

        "delete" -> do let i = read $ args !! 0
                       runTransaction db $ deleteItem i

        "list" -> do items <- runTransaction db listItems
                     mapM_ putStrLn items

    closeDatabase db

------------------------------------------------------------------------------

type Item = String
newtype List = List (TVar [TVar Item])

instance Database List where
    data Operation List = AddItem Item
                        | UpdateItem Int Item
                        | DeleteItem Int
                        deriving (Show, Read)

    encode = show
    decode = read

    perform (AddItem str) = addItem str
    perform (UpdateItem i str) = updateItem i str
    perform (DeleteItem i) = deleteItem i


addItem :: Item -> Transaction List ()
addItem item = do
    record (AddItem item)
    List listVar <- getData
    liftSTM $ do
        itemVar <- newTVar item
        itemsVar <- readTVar listVar
        writeTVar listVar (itemVar:itemsVar)

updateItem :: Int -> Item -> Transaction List ()
updateItem i str = do
    record (UpdateItem i str)
    List listVar <- getData
    liftSTM $ do
        itemsVar <- readTVar listVar
        let itemVar = itemsVar !! i
        writeTVar itemVar str

deleteItem :: Int -> Transaction List ()
deleteItem i = do
    record (DeleteItem i)
    List listVar <- getData
    liftSTM $ do
        items <- readTVar listVar
        when (i >= length items) (error "index out of bounds")
        let (xs,ys) = splitAt i items
            items' = xs ++ (tail ys)
        writeTVar listVar items'

listItems :: Transaction List [Item]
listItems = do
    List listVar <- getData
    liftSTM $ do
        itemVars <- readTVar listVar
        mapM readTVar itemVars

------------------------------------------------------------------------------

newtype Transaction d a = Transaction (StateT (Env d) STM a)
    deriving (Functor, Applicative, Monad)

type Env d = (d, Log d)
type Log d = [Operation d]

class Database d where
    data Operation d
    encode :: Operation d -> String
    decode :: String -> Operation d
    perform :: Operation d -> Transaction d ()

getData :: Transaction d d
getData = Transaction $ gets fst

record :: Operation d -> Transaction d ()
record op = Transaction $ modify' (\(d,log) -> (d,op:log))

liftSTM :: STM a -> Transaction d a
liftSTM m = Transaction $ lift m

------------------------------------------------------------------------------

runTransaction :: DatabaseHandle d -> Transaction d a -> IO a
runTransaction h m = atomicallyWithIO action finalizer
  where
    action = runT (base h) m
    finalizer (a,(_,log)) = serialize h log >> return a

runT :: d -> Transaction d a -> STM (a, Env d)
runT d (Transaction m) = runStateT m (d, [])

------------------------------------------------------------------------------

data DatabaseHandle d where
    DatabaseHandle :: Database d => { base :: d
                                    , logHandle :: Handle
                                    , logSem :: MVar ()
                                    } -> DatabaseHandle d

openDatabase :: Database d => d -> FilePath -> IO (DatabaseHandle d)
openDatabase base fp = do
    logHandle <- openFile fp ReadWriteMode
    logSem <- newMVar ()
    let d = DatabaseHandle{..}
    deserialize d
    return d

closeDatabase :: DatabaseHandle d -> IO ()
closeDatabase DatabaseHandle{..} = hClose logHandle

serialize :: DatabaseHandle d -> Log d -> IO ()
serialize DatabaseHandle{..} log =
    bracket_ (takeMVar logSem)
             (putMVar logSem ())
             (mapM_ (hPutStrLn logHandle . encode) (reverse log))

deserialize :: DatabaseHandle d -> IO ()
deserialize DatabaseHandle{..} = hMapLines_ logHandle replay
  where
    replay = void . atomically . runT base . perform . decode

hMapLines_ :: Handle -> (String -> IO ()) -> IO ()
hMapLines_ h f = do eof <- hIsEOF h
                    if eof
                        then return ()
                        else hGetLine h >>= f >> hMapLines_ h f


{-# WARNING atomicallyWithIO "unsupported; using unsafe simulation" #-}
atomicallyWithIO :: STM a -> (a -> IO b) -> IO b
atomicallyWithIO stm io = atomically stm >>= io

