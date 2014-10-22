{-# LANGUAGE FlexibleContexts #-}

module TX.LogFile (LogFile, openDatabase, closeDatabase) where

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString as B
import Data.SafeCopy
import Data.Serialize (Result(..), runGetPartial, runPut)
import System.IO

import TX

------------------------------------------------------------------------------

data LogFile = LogFile { fileHandle :: MVar Handle }

openDatabase :: (Database d, SafeCopy (Operation d))
             => FilePath -> d -> IO (DatabaseHandle LogFile d)
openDatabase fp d = do
    fileHandle <- mkFileHandle fp
    deserialize fileHandle d
    return DatabaseHandle { database   = d
                          , storage    = LogFile fileHandle
                          , serializer = serialize fileHandle
                          }

closeDatabase :: DatabaseHandle LogFile d -> IO ()
closeDatabase h = withMVar (fileHandle $ storage h) hClose

mkFileHandle :: FilePath -> IO (MVar Handle)
mkFileHandle fp = do
    h <- openBinaryFile fp ReadWriteMode
    hSetBuffering h NoBuffering
    newMVar h

deserialize :: (Database d, SafeCopy (Operation d))
            => MVar Handle -> d -> IO ()
deserialize fileHandle d =
    withMVar fileHandle (hMapDecode_ $ runTX_ d . replay)

serialize :: SafeCopy (Operation d)
          => MVar Handle -> [Operation d] -> IO ()
serialize fileHandle ops =
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
