{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module TX
    ( TX, Database(..), liftSTM, record, getData, throwTX, unsafeIOToTX
    , DatabaseHandle(..), openDatabase, closeDatabase
    , durably
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as B
import Data.SafeCopy
import Data.Serialize (Result(..), runGetPartial, runPut)
import GHC.Conc.Sync (unsafeIOToSTM, {- atomicallyWithIO -})
import System.IO

------------------------------------------------------------------------------

newtype TX d a = TX (StateT (d, [Operation d]) STM a)
  deriving (Functor, Applicative, Monad)

record :: Operation d -> TX d ()
record op = TX $ modify $ \(d,log) -> (d,op:log)

getData :: TX d d
getData = TX $ gets fst

liftSTM :: STM a -> TX d a
liftSTM = TX . lift

------------------------------------------------------------------------------

class Database d where
    data Operation d
    replay :: Operation d -> TX d ()


throwTX :: Exception e => e -> TX d a
throwTX = liftSTM . throwSTM

unsafeIOToTX :: IO a -> TX d a
unsafeIOToTX = liftSTM . unsafeIOToSTM

------------------------------------------------------------------------------

data DatabaseHandle d where
    DatabaseHandle :: (Database d, SafeCopy (Operation d))
                   => { database :: d
                      , fileHandle :: MVar Handle
                      } -> DatabaseHandle d

openDatabase :: (Database d, SafeCopy (Operation d))
             => FilePath -> d -> IO (DatabaseHandle d)
openDatabase fp d = do
    file <- openBinaryFile fp ReadWriteMode
    hSetBuffering file NoBuffering
    h <- DatabaseHandle <$> pure d <*> newMVar file
    deserialize h
    return h

deserialize :: DatabaseHandle d -> IO ()
deserialize (DatabaseHandle d h) =
    withMVar h (hMapDecode_ $ atomically' . replay)
  where
    atomically' (TX m) = void $ atomically $ runStateT m (d, [])

hMapDecode_ :: SafeCopy a => (a -> IO ()) -> Handle -> IO ()
hMapDecode_ f h = do c <- nextChunk
                     unless (B.null c) (go run c)
  where
    nextChunk = B.hGetSome h 8192
    run = runGetPartial safeGet
    go k c = case k c of
        Fail    msg _ -> error msg
        Partial k'    -> go k' =<< nextChunk
        Done    u c'  -> f u >> if B.null c'
                                    then hMapDecode_ f h
                                    else go run c'

closeDatabase :: DatabaseHandle d -> IO ()
closeDatabase (DatabaseHandle _ h) = withMVar h hClose

------------------------------------------------------------------------------

-- TODO: remove this
{-# WARNING durably "atomicallyWithIO not supported; using unsafe simulation" #-}
durably :: DatabaseHandle d -> TX d a -> IO a
durably h (TX m) = atomicallyWithIO action finalizer
  where
    action                 = runStateT m (database h, [])
    finalizer (a, (_,ops)) = serialize ops h >> return a

serialize :: [Operation d] -> DatabaseHandle d -> IO ()
serialize ops (DatabaseHandle _ h) =
    withMVar h (\h -> forM_ ops $ B.hPut h . runPut . safePut)

------------------------------------------------------------------------------

-- TODO: remove this
atomicallyWithIO :: STM a -> (a -> IO b) -> IO b
atomicallyWithIO stm io = atomically stm >>= io
