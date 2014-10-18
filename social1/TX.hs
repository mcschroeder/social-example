 {-# LANGUAGE FlexibleContexts #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}
 {-# LANGUAGE TypeFamilies #-}

module TX
    ( TX, Database(..)
    , record, getData, liftSTM, throwTX, unsafeIOToTX
    , DatabaseHandle, runTX
    , Transient, newTransientDatabase
    , LogFile, openDatabase, closeDatabase
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as B
import Data.SafeCopy
import Data.Serialize (Result(..), runGetPartial, runPut)
import GHC.Conc.Sync (unsafeIOToSTM)
import System.IO

------------------------------------------------------------------------------

newtype TX d a = TX (StateT (d, [Operation d]) STM a)
  deriving (Functor, Applicative, Monad)

class Database d where
    data Operation d
    replay :: Operation d -> TX d ()

record :: Operation d -> TX d ()
record = TX . modify' . second . (:)

getData :: TX d d
getData = TX $ fst <$> get

liftSTM :: STM a -> TX d a
liftSTM = TX . lift

throwTX :: Exception e => e -> TX d a
throwTX = liftSTM . throwSTM

unsafeIOToTX :: IO a -> TX d a
unsafeIOToTX = liftSTM . unsafeIOToSTM

------------------------------------------------------------------------------

data DatabaseHandle s d = DatabaseHandle
    { database  :: d
    , storage   :: s
    , serialize :: [Operation d] -> IO ()
    }

runTX :: DatabaseHandle s d -> TX d a -> IO a
runTX h (TX m) = atomicallyWithIO action finalizer
  where
    action = runStateT m (database h, [])
    finalizer (a, (_, ops)) = do serialize h (reverse ops)
                                 return a

runTX_ :: d -> TX d a -> IO ()
runTX_ d (TX m) = void $ atomically $ runStateT m (d, [])

{-# WARNING atomicallyWithIO "unsupported; using unsafe simulation" #-}
atomicallyWithIO :: STM a -> (a -> IO b) -> IO b
atomicallyWithIO stm io = atomically stm >>= io

------------------------------------------------------------------------------

data Transient = Transient

newTransientDatabase :: d -> IO (DatabaseHandle Transient d)
newTransientDatabase d =
    return DatabaseHandle { database = d
                          , storage = Transient
                          , serialize = const $ return ()
                          }

------------------------------------------------------------------------------

data LogFile = LogFile { fileHandle :: MVar Handle }

openDatabase :: (Database d, SafeCopy (Operation d))
             => FilePath -> d -> IO (DatabaseHandle LogFile d)
openDatabase fp d = do
    fileHandle <- mkFileHandle fp
    deserializeLog fileHandle d
    return DatabaseHandle { database  = d
                          , storage   = LogFile fileHandle
                          , serialize = serializeLog fileHandle
                          }

closeDatabase :: DatabaseHandle LogFile d -> IO ()
closeDatabase h = withMVar (fileHandle $ storage h) hClose

mkFileHandle :: FilePath -> IO (MVar Handle)
mkFileHandle fp = do
    h <- openBinaryFile fp ReadWriteMode
    hSetBuffering h NoBuffering
    newMVar h

deserializeLog :: (Database d, SafeCopy (Operation d))
            => MVar Handle -> d -> IO ()
deserializeLog fileHandle d =
    withMVar fileHandle (hMapDecode_ $ runTX_ d . replay)

serializeLog :: SafeCopy (Operation d)
             => MVar Handle -> [Operation d] -> IO ()
serializeLog fileHandle ops =
    withMVar fileHandle (\h -> forM_ ops $ B.hPut h . runPut . safePut)

hMapDecode_ :: SafeCopy a => (a -> IO ()) -> Handle -> IO ()
hMapDecode_ f h = do c <- nextChunk
                     unless (B.null c) (go run c)
  where
    nextChunk = B.hGetSome h 1024
    run = runGetPartial safeGet
    go k c = case k c of
        Fail    msg _ -> error msg
        Partial k'    -> go k' =<< nextChunk
        Done    u c'  -> f u >> if B.null c'
                                    then hMapDecode_ f h
                                    else go run c'
