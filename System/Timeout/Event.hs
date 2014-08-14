{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module System.Timeout.Event where

import qualified GHC.Event

data TimerManager2 a = TimerManager2
  { timerManager :: a
  , registerTimeout   :: a -> Int -> GHC.Event.TimeoutCallback -> IO GHC.Event.TimeoutKey
  , updateTimeout2    :: a -> GHC.Event.TimeoutKey -> Int -> IO ()
  , unregisterTimeout :: a -> GHC.Event.TimeoutKey -> IO ()
  }

data SomeTimerManager2 where
  SomeTimerManager2 :: TimerManager2 a -> SomeTimerManager2

newTimeoutManager2 :: IO SomeTimerManager2
newTimeoutManager2 = do
  tm <- GHC.Event.getSystemTimerManager
  return $! (SomeTimerManager2 (TimerManager2 tm GHC.Event.registerTimeout GHC.Event.updateTimeout GHC.Event.unregisterTimeout))

withTimeoutManager2 :: forall r . (forall a . TimerManager2 a -> IO r) -> IO r
withTimeoutManager2 f = do
  SomeTimerManager2 tm' <- newTimeoutManager2
  f tm'
