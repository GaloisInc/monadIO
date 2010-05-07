--------------------------------------------------------------------------
{- |
Module      : Control.Concurrent.MonadIO
Copyright   : (c) 2010 Galois, Inc.
License     : BSD-style (see the file libraries/base/LICENSE)

Maintainer  : John Launchbury, john@galois.com
Stability   : experimental
Portability : concurrency

Overloads the standard operations on MVars, Chans, and threads,
as defined in Control.Concurrent. This module is name-for-name
swappable with Control.Concurrent unless ghc-specific 
operations like 'mergeIO' or 'threadWaitRead' are used.

The standard operations on 'MVar' and 'Chan' (such as
'newEmptyMVar', or 'putChan') are overloaded over the
'MonadIO' class. A monad @m@ is declared an instance of
'MonadIO' by defining a function

> liftIO :: IO a -> m a

The explicit concurrency operations over threads are
available if a monad @m@ is declared an instance of the
'HasFork' class, by defining a function

> fork :: m () -> m ThreadId


 * Example use.

Suppose you define a new monad (EIO say) which is like
'IO' except that it provides an environment too.
You will need to declare EIO and instance of the 'Monad' class. In 
addition, you can declare it in the 'MonadIO' class. For example:

>  newtype EIO a = EIO {useEnv :: Env -> IO a}
>	
>  instance MonadIO EIO where
>    liftIO m = EIO $ (\_ -> m)

Now the standard operations on 'MVar' and 'Chan' (such as
'newEmptyMVar', or 'putChan' are immediately available as
EIO operations. To enable EIO to fork explicit threads, and to
access operations such as 'killThread' and 'threadDelay', use
the declaration

>  instance HasFork EIO where
>    fork em = EIO $ \e -> forkIO (em `useEnv` e)



 * Notes.

The 'MVar' operations do not include: withMVar, modifyMVar, or 
addMVarFinalizer. Consider using TMVars for these instead. In particular,
modifyMVar seems to promise atomicity, but it is NOT atomic. In
contrast TMVars can be used just like MVars, and they
will behave the way you expect (module Control.Concurrent.STM.MonadIO).

-}



--------------------------------------------------------------------------


{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}


module Control.Concurrent.MonadIO (
    MonadIO(..)
    
  , C.MVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , isEmptyMVar
  
  , C.Chan
  , newChan
  , writeChan
  , readChan
  , dupChan
  , unGetChan
  , isEmptyChan
  , getChanContents
  , writeList2Chan
  
  , HasFork(..)
  , C.ThreadId
  , forkIO
  , myThreadId
  , killThread
  , throwTo
  , yield
  , threadDelay
  
  )
  where

import Control.Monad.Trans(MonadIO(..))
import qualified Control.Concurrent as C
import qualified Control.Exception as E



newEmptyMVar  :: MonadIO io => io (C.MVar a)
newEmptyMVar   = liftIO $ C.newEmptyMVar

newMVar       :: MonadIO io => a -> io (C.MVar a)
newMVar x      = liftIO $ C.newMVar x

takeMVar      :: MonadIO io => C.MVar a -> io a
takeMVar m     = liftIO $ C.takeMVar m

putMVar       :: MonadIO io => C.MVar a -> a -> io ()
putMVar m x    = liftIO $ C.putMVar m x

readMVar      :: MonadIO io => C.MVar a -> io a
readMVar m     = liftIO $ C.readMVar m

swapMVar      :: MonadIO io => C.MVar a -> a -> io a
swapMVar m x   = liftIO $ C.swapMVar m x 

tryTakeMVar   :: MonadIO io => C.MVar a -> io (Maybe a)
tryTakeMVar m  = liftIO $ C.tryTakeMVar m

tryPutMVar    :: MonadIO io => C.MVar a -> a -> io Bool
tryPutMVar m x = liftIO $ C.tryPutMVar m x

isEmptyMVar   :: MonadIO io => C.MVar a -> io Bool
isEmptyMVar m  = liftIO $ C.isEmptyMVar m



-------------------------------------------------------------------------

newChan            :: MonadIO io => io (C.Chan a)
newChan             = liftIO $ C.newChan

writeChan          :: MonadIO io => C.Chan a -> a -> io ()
writeChan c x       = liftIO $ C.writeChan c x

readChan           :: MonadIO io => C.Chan a -> io a
readChan c          = liftIO $ C.readChan c

dupChan            :: MonadIO io => C.Chan a -> io (C.Chan a)
dupChan c           = liftIO $ C.dupChan c 

unGetChan          :: MonadIO io => C.Chan a -> a -> io ()
unGetChan c x       = liftIO $ unGetChan c x

isEmptyChan        :: MonadIO io => C.Chan a -> io Bool
isEmptyChan c       = liftIO $ C.isEmptyChan c

getChanContents    :: MonadIO io => C.Chan a -> io [a]
getChanContents c   = liftIO $ C.getChanContents c

writeList2Chan     :: MonadIO io => C.Chan a -> [a] -> io ()
writeList2Chan c xs = liftIO $ C.writeList2Chan c xs


-------------------------------------------------------------------------

class MonadIO io => HasFork io where
  fork :: io () -> io C.ThreadId

instance HasFork IO where
  fork m = C.forkIO m

-- | Included to maintain name-for-name compatibility
--   with Control.Concurrent
forkIO :: IO () -> IO C.ThreadId
forkIO = C.forkIO

myThreadId   :: HasFork io => io C.ThreadId
myThreadId    = liftIO $ C.myThreadId

killThread   :: HasFork io => C.ThreadId -> io ()
killThread i  = liftIO $ C.killThread i

throwTo      :: (E.Exception e, HasFork io) => C.ThreadId -> e -> io ()
throwTo i e   = liftIO $ C.throwTo i e

yield        :: HasFork io => io ()
yield         = liftIO $ C.yield

threadDelay  :: HasFork io => Int -> io ()
threadDelay n = liftIO $ C.threadDelay n


