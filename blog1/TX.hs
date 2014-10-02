{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module TX
    ( TX, Durable(..)
    , record, getData, liftSTM, throwTX, unsafeIOToTX
    , runTX
    , Database, openDatabase, closeDatabase
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import GHC.Conc.Sync (unsafeIOToSTM)
import System.IO

------------------------------------------------------------------------------

newtype TX d a = TX (StateT (d, [Operation d]) STM a)
  deriving (Functor, Applicative, Monad)

class Durable d where
    data Operation d
    encode :: Operation d -> ByteString
    decode :: ByteString -> Operation d
    replay :: Operation d -> TX d ()

record :: Durable d => Operation d -> TX d ()
record op = TX $ modify' (second (op:))

getData :: TX d d
getData = TX $ fst <$> get

liftSTM :: STM a -> TX d a
liftSTM = TX . lift

throwTX :: Exception e => e -> TX d a
throwTX = liftSTM . throwSTM

unsafeIOToTX :: IO a -> TX d a
unsafeIOToTX = liftSTM . unsafeIOToSTM

------------------------------------------------------------------------------

runTX :: Durable d => Database d -> TX d a -> IO a
runTX db (TX m) = atomicallyWithIO action finalizer
  where
    action = runStateT m (userData db, [])
    finalizer (a, (_, ops)) = serialize db (reverse ops) >> return a

runTX_ :: Database d -> TX d a -> IO ()
runTX_ db (TX m) = void $ atomically $ runStateT m (userData db, [])

{-# WARNING atomicallyWithIO "unsupported; using unsafe simulation" #-}
atomicallyWithIO :: STM a -> (a -> IO b) -> IO b
atomicallyWithIO stm io = atomically stm >>= io

------------------------------------------------------------------------------

data Database d = Database
    { userData :: d
    , fileHandle :: MVar Handle
    }

openDatabase :: Durable d => FilePath -> d -> IO (Database d)
openDatabase fp userData = do
    fileHandle <- newMVar =<< openFile fp ReadWriteMode
    let db = Database{..}
    deserialize db
    return db

closeDatabase :: Database d -> IO ()
closeDatabase db = withMVar (fileHandle db) hClose

serialize :: Durable d => Database d -> [Operation d] -> IO ()
serialize db ops =
    withMVar (fileHandle db) (\h -> mapM_ (C.hPutStrLn h . encode) ops)

deserialize :: Durable d => Database d -> IO ()
deserialize db =
    withMVar (fileHandle db) (hMapLines_ (runTX_ db . replay . decode))

hMapLines_ :: (ByteString -> IO ()) -> Handle -> IO ()
hMapLines_ f h = do eof <- hIsEOF h
                    unless eof $ do
                        str <- C.hGetLine h
                        f str
                        hMapLines_ f h
