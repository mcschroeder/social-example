{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO

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

newtype Transaction d a = Transaction { unT :: StateT (Env d) STM a }
    deriving (Functor, Applicative, Monad)

type Env d = (d, Log d)
type Log d = [Operation d]

class Database d where
    data Operation d
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
    action = runStateT (unT m) (base h, [])
    finalizer (a,(_,log)) = serialize h (reverse log) >> return a

------------------------------------------------------------------------------

data DatabaseHandle d = DatabaseHandle
    { base :: d
    , logHandle :: Handle
    }

openDatabase :: Database d => d -> FilePath -> IO (DatabaseHandle d)
openDatabase = undefined

closeDatabase :: DatabaseHandle d -> IO ()
closeDatabase = undefined

-- note: needs to ensure only 1 thread writes
serialize :: DatabaseHandle d -> Log d -> IO ()
serialize = undefined

deserialize :: DatabaseHandle d -> IO ()
deserialize = undefined

atomicallyWithIO :: STM a -> (a -> IO b) -> IO b
atomicallyWithIO = undefined

