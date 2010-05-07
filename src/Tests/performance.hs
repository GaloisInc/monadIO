

module Main (

  main

  ) where

import Data.IORef.MonadIO
import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO


(newV,takeV,putV,modify,style)
--     = (newIORef,readIORef,writeIORef,modifyV,"IORef")
--     = (newMVar,takeMVar,putMVar,modifyV,"MVar")
--     = (newTVar,readTVar,writeTVar,modifyV,"TVar")
--     = (newTVar,readTVar,writeTVar,modifyTV,"TVar (atomic)")
--     = (newTMVar,takeTMVar,putTMVar,modifyV,"TMVar")
     = (newTMVar,takeTMVar,putTMVar,modifyTM,"TMVar (atomic)")


main = do
    t <- newV 0
    let loop 0 = return ()
        loop n = modify t (+1) >> loop (n-1)
    loop 100000000
    n<-takeV t
    putStrLn ("Done: " ++ style ++ " - " ++ show n)

modifyV t f = do
    x <- takeV t
    let y = f x
    seq y $ putV t y

modifyTV t f = atomically $ do
    x <- readTVarSTM t
    let y = f x
    seq y $ writeTVarSTM t y

modifyTM t f = atomically $ do
    x <- takeTMVarSTM t
    let y = f x
    seq y $ putTMVarSTM t y

{-

<Test 1> - 1x

Done: IORef - 100000000
real	0m1.808s
user	0m1.787s
sys	0m0.021s

Done: IORef - 100000000
real	0m1.794s
user	0m1.784s
sys	0m0.009s

<Test 2> - 0.96x

Done: MVar - 100000000
real	0m1.735s
user	0m1.725s
sys	0m0.009s

Done: MVar - 100000000
real	0m1.734s
user	0m1.724s
sys	0m0.009s

<Test 3> - 5.6x

Done: TVar - 100000000
real	0m10.067s
user	0m9.985s
sys	0m0.081s

Done: TVar - 100000000
real	0m10.060s
user	0m10.011s
sys	0m0.049s

<Test 4> - 5x

Done: TVar - 100000000
real	0m9.001s
user	0m8.985s
sys	0m0.016s

Done: TVar - 100000000
real	0m9.064s
user	0m8.991s
sys	0m0.073s

<Test 5> - 27x

Done: TMVar - 100000000
real	0m24.259s
user	0m24.156s
sys	0m0.104s

Done: TMVar - 100000000
real	0m24.337s
user	0m24.148s
sys	0m0.190s

<Test 6> - 16.7x

Done: TMVar (atomic) - 100000000
real	0m14.973s
user	0m14.946s
sys	0m0.028s

Done: TMVar (atomic) - 100000000
real	0m14.995s
user	0m14.928s
sys	0m0.068s


<Test 1 and 2 without overloading...> - unchanged

Done: IORef - 100000000
real	0m1.796s
user	0m1.786s
sys	0m0.009s

Done: IORef - 100000000
real	0m1.799s
user	0m1.789s
sys	0m0.009s

Done: MVar - 100000000
real	0m1.737s
user	0m1.726s
sys	0m0.009s

Done: MVar - 100000000
real	0m1.734s
user	0m1.724s
sys	0m0.009s

-}
