--------------------------------------------------------------------------
{- |
Module      : Data.IORef.MonadIO
Copyright   : (c) 2010 Galois, Inc.
License     : BSD-style (see the file libraries/base/LICENSE)

Maintainer  : John Launchbury, john@galois.com
Stability   : experimental
Portability : IO

Overloads the standard operations on IORefs,
as defined in Data.IORef. This module is name-for-name
swappable with Data.IORef unless ghc-specific 
operations like weak pointers are used.

The standard operations on 'IORef' (such as
'newIORef', or 'modifyIORef') are overloaded over the
'MonadIO' class. A monad @m@ is declared an instance of
'MonadIO' by defining a function

> liftIO :: IO a -> m a

-}

--------------------------------------------------------------------------



module Data.IORef.MonadIO (
    MonadIO(..)
    
  , R.IORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , atomicModifyIORef
  
  )
  where

import Control.Monad.Trans(MonadIO(..))
import qualified Data.IORef as R


newIORef :: MonadIO io => a -> io (R.IORef a)
newIORef x = liftIO $ R.newIORef x

readIORef :: MonadIO io => R.IORef a -> io a
readIORef r = liftIO $ R.readIORef r

writeIORef :: MonadIO io => R.IORef a -> a -> io ()
writeIORef r x = liftIO $ R.writeIORef r x

modifyIORef :: MonadIO io => R.IORef a -> (a -> a) -> io ()
modifyIORef r f = liftIO $ R.modifyIORef r f

atomicModifyIORef :: MonadIO io => R.IORef a -> (a -> (a, b)) -> io b
atomicModifyIORef r f = liftIO $ R.atomicModifyIORef r f

