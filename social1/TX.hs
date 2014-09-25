{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module TX
    ( TX
    , Durable(..), record
    , getDatabase, liftSTM, throwTX, unsafeIOToTX
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import GHC.Conc.Sync (unsafeIOToSTM)

newtype TX d a = TX (StateT (d, [Operation d]) STM a)
  deriving (Functor, Applicative, Monad)

class Durable d where
    data Operation d
    encode :: Operation d -> ByteString
    decode :: ByteString -> Operation d
    replay :: Operation d -> TX d ()

record :: Durable d => Operation d -> TX d ()
record op = TX $ modify' (second (op:))

getDatabase :: TX d d
getDatabase = TX $ fst <$> get

liftSTM :: STM a -> TX d a
liftSTM = TX . lift

throwTX :: Exception e => e -> TX d a
throwTX = liftSTM . throwSTM

unsafeIOToTX :: IO a -> TX d a
unsafeIOToTX = liftSTM . unsafeIOToSTM
