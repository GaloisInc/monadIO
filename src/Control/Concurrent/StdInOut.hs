--------------------------------------------------------------------------
{- |
Module      : Control.Concurrent.StdInOut
Copyright   : (c) 2010 Galois, Inc.
License     : BSD-style (see the file libraries/base/LICENSE)

Maintainer  : John Launchbury, john@galois.com
Stability   : experimental
Portability : concurrency

A low-tech concurrent interface to the console. When multiple threads
want input, they send messages to the console with the format

>  <thread-id>:request

The user supplies input to any requesting thread in a similar way:

>  <thread-id>:response

At any time, the user can enter @!!@ to obtain a listing of all the
active prompts. Any input not of either of these forms is discarded.

> example :: IO ()
> example = setupStdinout processes
> 
> processes :: IO ()
> processes = do
>     forkIO $ (prompt "Enter something" >> return ())
>     forkIO $ (prompt "Something else" >> return ())
>     prompt "quit"      -- When the main thread dies,
>     return ()          -- the whole interaction ends

-}

--------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Control.Concurrent.StdInOut
  ( setupStdInOut  -- :: IO a -> IO a
  , prompt         -- :: String -> IO String
  , putStrLine     -- :: String -> IO ()
  ) where


import System.IO(hSetBuffering,BufferMode(..),stdout)
import System.IO.Unsafe(unsafePerformIO) -- for a global variable
import Control.Monad(forever,join)
import Control.Exception(finally)

import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO


flag :: String
flag = "!!"                          -- User input to list active prompts
timeDelay :: Int
timeDelay = 100000                   -- 1/10th sec


-- 'std' is a concurrent variable that holds the current input line for
-- other processes to consider whether they want to grab it. The
-- process 'forever inputScan' maintains it.

type Stdinout = TMVar String

{-# NOINLINE std #-}
std :: Stdinout
std = unsafePerformIO $ newEmptyTMVar  -- global location for input strings

-- | 'setupStdInOut' establishes the context for 'prompt', by running
--   a daemon while its argument is executing. The daemon is terminated
--   once the argument to 'setupStdInOut' finishes.

setupStdInOut :: IO a -> IO a
setupStdInOut procs = do
    hSetBuffering stdout LineBuffering   -- prevents character interleaving
    tid <- fork $ forever inputScan      -- daemon putting stdin into std
    procs `finally` killThread tid

inputScan :: IO (Maybe String)
inputScan = do
    str <- getLine                       -- get next input line from stdin
    putTMVar std str                     -- make the input available to others
    threadDelay timeDelay                -- give others time to grab it
    tryTakeTMVar std                     -- clear the input if still present


-- | 'prompt' is the main user level function of the module. The function
-- prints its argument on stdout, prefixed by its process number. The user 
-- similarly selects the recipient by prefixing the process number,
-- e.g. "23:". Active prompts will reprompt when !! is entered. 

prompt :: HasFork io => String -> io String
prompt text = do
    name <- myThreadNumber
    putStrLine (name ++ text)
    (join . atomically) $ do
       str <- takeTMVarSTM std           -- Grab the input string to examine
       case match name str of
         Just inp -> return (return inp) -- Exit with the string contents
         Nothing -> do
            check (str==flag)            -- Test whether to reprint the prompt
            putTMVarSTM std flag         -- Replace the flag for others to see
            return $ do
               threadDelay (2*timeDelay) -- Wait for the flag to flush
               prompt text               -- Recurse, to reprint the prompt

myThreadNumber :: HasFork io => io String
myThreadNumber = do
    tid <- myThreadId
    return $ drop (length "ThreadId ") (show tid ++ ":")

match :: String -> String -> Maybe String
match n s = if n == take (length n) s then
              Just (drop (length n) s)
            else
              Nothing

-- | 'putStrLine' sends output to stdout, ensuring that lines are whole
-- and uninterrupted (including the final newline).

putStrLine :: MonadIO io => String -> io ()
putStrLine s = liftIO $ putStr (s ++ "\n")  

