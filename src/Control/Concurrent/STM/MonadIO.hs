--------------------------------------------------------------------------
{- |
Module      : Control.Concurrent.STM.MonadIO
Copyright   : (c) 2010 Galois, Inc.
License     : BSD-style (see the file libraries/base/LICENSE)

Maintainer  : John Launchbury, john@galois.com
Stability   : experimental
Portability : concurrency, requires STM

Overloads the standard operations on TVars, and TMVars as defined
in Control.Concurrent.STM.

TVars and MVars are often thought of as variables to be 
used in the STM monad. But in practice, they should be used
just as frequently (if not more so) in any IO-like monad, with STM 
being used purely when a new atomic transaction is being defined.
Thus we reverse the naming convention, and use
the plain access names when in the IO-like monad, and use an explicit STM 
suffix when using the variables tentatively within the STM monad itself.

TMVars are particularly valuable when used in an IO-like monad,
because operations like readTMVar and modifyTMvar
can guarantee the atomicity of the operation (unlike the corresponding
operations over MVars).

The standard operations on 'TVar' and 'TMVar' (such as
'writeTVar' or 'newEmptyTMVar') are overloaded over the
'MonadIO' class. A monad @m@ is declared an instance of
'MonadIO' by defining a function

> liftIO :: IO a -> m a

It also overloads the 'atomically' function, so that STM transactions
can be defined from within any MonadIO monad.

-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}



module Control.Concurrent.STM.MonadIO (

    STM
  , atomically
  , retry
  , orElse
  , check
  , catchSTM

  , S.TVar
  , newTVar
  , readTVar
  , writeTVar
  , registerDelay
  , modifyTVar
  , modifyTVar_

  , newTVarSTM
  , readTVarSTM
  , writeTVarSTM


  , S.TMVar
  , newTMVar
  , newEmptyTMVar
  , takeTMVar
  , putTMVar
  , readTMVar
  , swapTMVar
  , tryTakeTMVar
  , tryPutTMVar
  , isEmptyTMVar
  , modifyTMVar
  , modifyTMVar_

  , newTMVarSTM
  , newEmptyTMVarSTM
  , takeTMVarSTM
  , putTMVarSTM
  , readTMVarSTM
  , swapTMVarSTM
  , tryTakeTMVarSTM
  , tryPutTMVarSTM
  , isEmptyTMVarSTM
  )
  where

import Control.Monad.STM hiding (atomically)
import qualified Control.Concurrent.STM as S
import Control.Concurrent.MonadIO

import GHC.Conc (readTVarIO)


-------------------------------------------------------------------------
-- | The atomically function allows STM to be called directly from any
-- monad which contains IO, i.e. is a member of MonadIO.

atomically :: MonadIO io => STM a -> io a
atomically m = liftIO $ S.atomically m

-------------------------------------------------------------------------


type TVar = S.TVar

newTVar      :: MonadIO io => a -> io (TVar a)
newTVar x     = liftIO $ S.newTVarIO x

readTVar     :: MonadIO io => TVar a -> io a
readTVar t    = liftIO $ readTVarIO t

writeTVar    :: MonadIO io => TVar a -> a -> io ()
writeTVar t x = atomically $ writeTVarSTM t x

registerDelay :: MonadIO io => Int -> io (TVar Bool)
registerDelay n = liftIO $ S.registerDelay n

-- | 'modifyTVar' is an atomic update operation which provides both
-- the former value and the newly computed value as a result.

modifyTVar :: MonadIO io => TVar a -> (a -> a) -> io (a,a)
modifyTVar t f = atomically $ do
                    x <- readTVarSTM t
                    let y = f x
                    seq y $ writeTVarSTM t y
                    return (x,y)

modifyTVar_ :: MonadIO io => TVar a -> (a -> a) -> io ()
modifyTVar_ t f = atomically $ do
                    x <- readTVarSTM t
                    let y = f x
                    seq y $ writeTVarSTM t y

----------------------


newTVarSTM :: a -> STM (TVar a)
newTVarSTM x = S.newTVar x

readTVarSTM :: TVar a -> STM a
readTVarSTM t = S.readTVar t

writeTVarSTM :: TVar a -> a -> STM ()
writeTVarSTM t x = S.writeTVar t x




-------------------------------------------------------------------------

type TMVar a = S.TMVar a

newTMVar :: MonadIO io => a -> io (TMVar a)
newTMVar x = liftIO $ S.newTMVarIO x

newEmptyTMVar :: MonadIO io => io (TMVar a)
newEmptyTMVar = liftIO $ S.newEmptyTMVarIO

takeTMVar :: MonadIO io => TMVar a -> io a
takeTMVar t = atomically $ takeTMVarSTM t

putTMVar :: MonadIO io => TMVar a -> a -> io ()
putTMVar t x  = atomically $ putTMVarSTM t x

readTMVar :: MonadIO io => TMVar a -> io a
readTMVar t = atomically $ readTMVarSTM t

swapTMVar :: MonadIO io => TMVar a -> a -> io a
swapTMVar t x = atomically $ swapTMVarSTM t x

tryTakeTMVar :: MonadIO io => TMVar a -> io (Maybe a)
tryTakeTMVar t = atomically $ tryTakeTMVarSTM t

tryPutTMVar :: MonadIO io => TMVar a -> a -> io Bool
tryPutTMVar t x = atomically $ tryPutTMVarSTM t x

isEmptyTMVar :: MonadIO io => TMVar a -> io Bool
isEmptyTMVar t = atomically $ isEmptyTMVarSTM t

-- modifyTMVar is an atomic update operation which provides both
-- the former value and the newly computed value as a result.

modifyTMVar :: MonadIO io => TMVar a -> (a -> a) -> io (a,a)
modifyTMVar t f = atomically $ do
                    x <- takeTMVarSTM t
                    let y = f x
                    seq y $ putTMVarSTM t y
                    return (x,y)

modifyTMVar_ :: MonadIO io => TMVar a -> (a -> a) -> io ()
modifyTMVar_ t f = atomically $ do
                    x <- takeTMVarSTM t
                    let y = f x
                    seq y $ putTMVarSTM t y

----------------------

newTMVarSTM :: a -> STM (TMVar a)
newTMVarSTM = S.newTMVar

newEmptyTMVarSTM :: STM (TMVar a)
newEmptyTMVarSTM = S.newEmptyTMVar

takeTMVarSTM :: TMVar a -> STM a
takeTMVarSTM = S.takeTMVar

putTMVarSTM :: TMVar a -> a -> STM ()
putTMVarSTM = S.putTMVar

readTMVarSTM :: TMVar a -> STM a
readTMVarSTM = S.readTMVar

swapTMVarSTM :: TMVar a -> a -> STM a
swapTMVarSTM = S.swapTMVar

tryTakeTMVarSTM :: TMVar a -> STM (Maybe a)
tryTakeTMVarSTM = S.tryTakeTMVar

tryPutTMVarSTM :: TMVar a -> a -> STM Bool
tryPutTMVarSTM = S.tryPutTMVar

isEmptyTMVarSTM :: TMVar a -> STM Bool
isEmptyTMVarSTM = S.isEmptyTMVar

