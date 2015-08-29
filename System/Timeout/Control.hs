{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Add updatable timeout functionality to a 'Control.Monad.Monad' transformer stack layered on 'System.IO.IO'

module System.Timeout.Control
  ( runTimeout
  -- , disableTimeout
  , updateTimeout
  , Timeout
  , TimeoutException(..)
  , Microseconds(..)
  ) where

import Control.Applicative
import Control.Concurrent (myThreadId)
import Control.Exception (Exception, throwTo)
import Control.Exception.Lifted (try)
import Control.Monad (liftM)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import Data.Typeable (Typeable)
import Data.Unique (Unique, newUnique)
import qualified GHC.Event as E (TimeoutKey)
import System.Timeout.Event

-- |
-- A duration measured in microseconds
newtype Microseconds = Microseconds Int
  deriving (Num, Show)

data TimeoutException
  = TimeoutException Unique            -- ^ A timeout occurred
  | MissingSystemEventManagerException -- ^ The system event manager was unavailable
    deriving (Eq, Typeable)

instance Exception TimeoutException

instance Show TimeoutException where
  show TimeoutException{} = "TimeoutException"
  show MissingSystemEventManagerException{} = "MissingSystemEventManagerException"


data TimeoutState = TimeoutState {timeoutManager :: SomeTimerManager2, timeoutKey :: E.TimeoutKey}

newtype Timeout m a = Timeout {unTimeout :: ReaderT TimeoutState m a}
  deriving (Applicative, Functor, Monad, MonadReader TimeoutState, MonadIO, MonadTrans)

instance MonadTransControl Timeout where
  type StT Timeout a = StT (ReaderT TimeoutState) a
  liftWith = defaultLiftWith Timeout unTimeout
  restoreT = defaultRestoreT Timeout

instance MonadBase b m => MonadBase b (Timeout m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (Timeout m) where
  type StM (Timeout m) a = ComposeSt Timeout m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM


-- |
-- Run the timeout transformer
runTimeout
  :: (MonadBaseControl IO m, MonadIO m)
  => Microseconds -- ^ Microseconds in the future
  -> Timeout m a  -- ^ Timeout action to run
  -> m (Either TimeoutException a) -- ^ The result or a 'TimeoutException'
{-# INLINEABLE runTimeout #-}
runTimeout (Microseconds us) (Timeout action) = do
  tm2@(SomeTimerManager2 eventMgr) <- liftIO newTimeoutManager2
  state <- liftIO $ do
        tid <- myThreadId
        uni <- liftM TimeoutException newUnique
        key <- (registerTimeout eventMgr) (timerManager eventMgr) us (throwTo tid uni)
        return $! TimeoutState{timeoutManager = tm2, timeoutKey = key}
  try $ do
         val <- runReaderT action state
         unregisterTimeout_ state
         return $! val

-- |
-- Reset the timeout duration
updateTimeout
  :: MonadIO m
  => Microseconds -- ^ Microseconds in the future
  -> Timeout m ()
{-# INLINE updateTimeout #-}
updateTimeout (Microseconds us) = do
  TimeoutState{timeoutManager, timeoutKey} <- ask
  case timeoutManager of
    (SomeTimerManager2 tm) -> liftIO $ (updateTimeout2 tm) (timerManager tm) timeoutKey us

unregisterTimeout_ :: MonadIO m => TimeoutState -> m ()
unregisterTimeout_ TimeoutState{timeoutManager, timeoutKey} =
  case timeoutManager of
    (SomeTimerManager2 tm2) -> liftIO $ (unregisterTimeout tm2) (timerManager tm2) timeoutKey

{-
 -
 - NOTE: commented out until 'runTimeout' allows updating the timeout key

-- |
-- Disable timeouts for the remainder of execution.
disableTimeout
  :: MonadIO m
  => Timeout m ()
disableTimeout = do
  state <- ask
  unregisterTimeout_ state
-}

