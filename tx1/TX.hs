{-# LANGUAGE TypeFamilies #-}

module TX
    ( TX, Database(..), liftSTM, record, getData, throwTX, unsafeIOToTX
    , DatabaseHandle(..), runTX, runTX_
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import GHC.Conc.Sync (unsafeIOToSTM, {- atomicallyWithIO -})

------------------------------------------------------------------------------

data TX d a = TX { unTX :: d -> STM (a, [Operation d]) }

instance Functor (TX d) where
  fmap f m = TX $ \d -> do
    (a, ops) <- unTX m d
    return (f a, ops)

instance Applicative (TX d) where
  pure = return
  (<*>) = ap

instance Monad (TX d) where
  return a = TX $ \_ -> return (a, [])
  m >>= k = TX $ \d -> do
    (a, ops ) <- unTX m d
    (b, ops') <- unTX (k a) d
    return (b, ops ++ ops')

------------------------------------------------------------------------------

class Database d where
    data Operation d
    replay :: Operation d -> TX d ()

record :: Operation d -> TX d ()
record op = TX $ \_ -> return ((), [op])

getData :: TX d d
getData = TX $ \d -> return (d, [])

liftSTM :: STM a -> TX d a
liftSTM m = TX $ \_ -> m >>= \a -> return (a, [])

throwTX :: Exception e => e -> TX d a
throwTX = liftSTM . throwSTM

unsafeIOToTX :: IO a -> TX d a
unsafeIOToTX = liftSTM . unsafeIOToSTM

------------------------------------------------------------------------------

data DatabaseHandle s d = DatabaseHandle
    { database   :: d
    , storage    :: s
    , serializer :: [Operation d] -> IO ()
    }

-- TODO: remove this
{-# WARNING runTX "atomicallyWithIO not supported; using unsafe simulation" #-}
runTX :: DatabaseHandle s d -> TX d a -> IO a
runTX h m = atomicallyWithIO action finalizer
  where
    action = unTX m (database h)
    finalizer (a, ops) = serializer h ops >> return a

runTX_ :: d -> TX d a -> IO ()
runTX_ d m = void $ atomically $ unTX m d

------------------------------------------------------------------------------

-- TODO: remove this
atomicallyWithIO :: STM a -> (a -> IO b) -> IO b
atomicallyWithIO stm io = atomically stm >>= io
