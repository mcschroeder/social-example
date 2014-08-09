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
        "add"    -> runTransaction db $ newItem (args !! 0)
        "remove" -> runTransaction db $ deleteItem (args !! 0)
        "list"   -> do items <- runTransaction db listItems
                       mapM_ putStrLn items

    closeDatabase db

------------------------------------------------------------------------------

data Item = Item
    { itemId :: ItemId
    , itemText :: String
    , itemDone :: Bool
    } deriving (Show,Read)

type ItemId = Int

newtype List = List (TVar [TVar Item])

instance Database List where
    data Operation List = AddItem Item
                        | UpdateItem Item
                        | DeleteItem ItemId
                        deriving (Show, Read)

    encode = show
    decode = read

    perform (AddItem i) = addItem i
    perform (UpdateItem i) = updateItem i
    perform (DeleteItem i) = deleteItem i

addItem = undefined
updateItem = undefined
deleteItem = undefined
getItem = undefined
newItem = undefined
listItems = undefined

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

