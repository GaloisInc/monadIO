--------------------------------------------------------------------------
{- |
Module      : Main
Copyright   : (c) 2010 Galois, Inc.
License     : BSD-style (see the file libraries/base/LICENSE)

Maintainer  : John Launchbury, john@galois.com
Stability   : experimental
Portability : IO, Concurrency, STM

Tests the performance of different kinds of variables in
parallel code to understand the cost of the
variable independent of any blocking or rework.


-}

--------------------------------------------------------------------------



module Main where
 
import System.Environment (getArgs)
import System.Time
import System.Mem

import Data.IORef.MonadIO
import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO


work      = 20 :: Int  -- total work performed
threadMax = 15 :: Int  -- 2^threadMax threads


(newV,takeV,putV,modify,style)
     = (newIORef,readIORef,writeIORef,modifyIO,"IORef (atomic)")
--     = (newMVar,takeMVar,putMVar,modifyV,"MVar")
--     = (newTVar,readTVar,writeTVar,modifyTV,"TVar (atomic)")
--     = (newTMVar,takeTMVar,putTMVar,modifyTM,"TMVar (atomic)")

--  Unsafe variants for comparison
--     = (newIORef,readIORef,writeIORef,modifyV,"IORef")
--     = (newTVar,readTVar,writeTVar,modifyV,"TVar")
--     = (newTMVar,takeTMVar,putTMVar,modifyV,"TMVar")
 
 
main = do
    putStrLn "Threads, Size, Time, Style, Work, Correct"
    sequence_ [timing work b | b <- [0..threadMax]]

timing w b = do
    let threads = 2^b :: Int
        size    = 2^(work-b) :: Int
    performGC
    tStart <- getClockTime
    n <- task threads size
    tEnd <- getClockTime
    putStrLn (show b ++","++
              show (work-b) ++","++
              show (secDiff tStart tEnd) ++","++
              show style ++","++
              show n ++","++
              show (n==2^work))

task :: Int -> Int -> IO Int
task threads size = do
    t <- newV 0
    let loop 0 = return ()
        loop n = modify t (+1) >> loop (n-1)
    flags <- sequence [forkEnd $ loop size | i<-[1..threads]]
    sequence_ flags
    takeV t

forkEnd :: HasFork io => io a -> io (io ())
forkEnd m = do
  flag <- newEmptyMVar
  fork (m >> putMVar flag ())
  return (takeMVar flag)


modifyV t f = do
    x <- takeV t
    let y = f x
    seq y $ putV t y

modifyIO t f = do
    y <- atomicModifyIORef t (\x -> let x' = f x in (x',x'))
    seq y $ return ()

modifyTV t f = atomically $ do
    x <- readTVarSTM t
    let y = f x
    seq y $ writeTVarSTM t y

modifyTM t f = atomically $ do
    x <- takeTMVarSTM t
    let y = f x
    seq y $ putTMVarSTM t y


secDiff :: ClockTime -> ClockTime -> Double
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2)
  = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)


