{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.ByteString as B
import Data.SafeCopy
import Data.Serialize (Result(..), runGetPartial, runPut)
import GHC.Conc.Sync (unsafeIOToSTM)
import System.IO

------------------------------------------------------------------------------

newtype TX d a = TX (StateT (d, [Operation d]) STM a)
  deriving (Functor, Applicative, Monad)

class SafeCopy (Operation d) => Durable d where
    data Operation d
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
    h <- openBinaryFile fp ReadWriteMode
    hSetBuffering h NoBuffering
    fileHandle <- newMVar h
    let db = Database{..}
    eof <- hIsEOF h
    unless eof $ deserialize db
    return db

closeDatabase :: Database d -> IO ()
closeDatabase db = withMVar (fileHandle db) hClose

serialize :: Durable d => Database d -> [Operation d] -> IO ()
serialize db ops =
    withMVar (fileHandle db)
             (\h -> forM_ ops $ B.hPut h . runPut . safePut)

deserialize :: Durable d => Database d -> IO ()
deserialize db =
    withMVar (fileHandle db)
             (hMapDecode_ $ runTX_ db . replay)

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
