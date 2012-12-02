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
import qualified GHC.Event as E (EventManager, TimeoutKey, getSystemEventManager, registerTimeout, unregisterTimeout, updateTimeout)

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


data TimeoutState = TimeoutState {timeoutManager :: E.EventManager, timeoutKey :: E.TimeoutKey}

newtype Timeout m a = Timeout {unTimeout :: ReaderT TimeoutState m a}
  deriving (Applicative, Functor, Monad, MonadReader TimeoutState, MonadIO, MonadTrans)

instance MonadTransControl Timeout where
  newtype StT Timeout a = StTimeoutT {unStAction :: StT (ReaderT TimeoutState) a}
  liftWith f = Timeout $ liftWith $ \runReader' ->
                           f (liftM StTimeoutT . runReader' . unTimeout)
  restoreT = Timeout . restoreT . liftM unStAction

instance MonadBase b m => MonadBase b (Timeout m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (Timeout m) where
  newtype StM (Timeout m) a = StMT {unStMT :: ComposeSt Timeout m a}
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM     = defaultRestoreM   unStMT


-- |
-- Run the timeout transformer
runTimeout
  :: (MonadBaseControl IO m, MonadIO m)
  => Microseconds -- ^ Microseconds in the future
  -> Timeout m a  -- ^ Timeout action to run
  -> m (Either TimeoutException a) -- ^ The result or a 'TimeoutException'
{-# INLINEABLE runTimeout #-}
runTimeout (Microseconds us) (Timeout action) = do
  eventMgr <- liftIO $ E.getSystemEventManager
  case eventMgr of
    Nothing        -> return . Left $ MissingSystemEventManagerException
    Just eventMgr' -> do
      state <- liftIO $ do
        tid <- myThreadId
        uni <- liftM TimeoutException newUnique
        key <- E.registerTimeout eventMgr' us (throwTo tid uni)
        return $! TimeoutState{timeoutManager = eventMgr', timeoutKey = key}
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
  liftIO $ E.updateTimeout timeoutManager timeoutKey us

unregisterTimeout_ :: MonadIO m => TimeoutState -> m ()
unregisterTimeout_ TimeoutState{timeoutManager, timeoutKey} =
  liftIO $ E.unregisterTimeout timeoutManager timeoutKey

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

