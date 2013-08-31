{-# LANGUAGE ScopedTypeVariables #-}
--
-- Copyright (c) 2013   Stefan Wehr - http://www.stefanwehr.de
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA
--

module Test.Framework.ThreadPool (

    ThreadPoolEntry, ThreadPool(..), sequentialThreadPool, parallelThreadPool
  , threadPoolTest

) where

import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

-- for tests
import System.Random

type ThreadPoolEntry m a b = ( m a        -- pre-action, must not throw exceptions
                             , a -> IO b  -- action
                             , Either Ex.SomeException b -> m ()  -- post-action, must not throw exceptions
                             )

data ThreadPool m a b
    = ThreadPool
      { tp_run :: [ThreadPoolEntry m a b] -> m () }

sequentialThreadPool :: MonadIO m => ThreadPool m a b
sequentialThreadPool = ThreadPool runSequentially

parallelThreadPool :: MonadIO m => Int -> m (ThreadPool m a b)
parallelThreadPool n =
    do when (n < 1) $ fail ("invalid number of workers: " ++ show n)
       return (ThreadPool (runParallel n))

runSequentially :: MonadIO m => [ThreadPoolEntry m a b] -> m ()
runSequentially entries =
    mapM_ run entries
    where
      run (pre, action, post) =
          do a <- pre
             b <- liftIO $ Ex.try (action a)
             post b

data WorkItem m b = Work (IO b) (Either Ex.SomeException b -> m ()) | Done

instance Show (WorkItem m b) where
    show (Work _ _) = "Work"
    show Done = "Done"

type NamedMVar a = (String, MVar a)
type NamedChan a = (String, Chan a)

type ToWorker m b = NamedMVar (WorkItem m b)

data WorkResult m b = WorkResult (m ()) (ToWorker m b)

instance Show (WorkResult m b) where
    show _ = "WorkResult"

type FromWorker m b = NamedChan (WorkResult m b)

runParallel :: forall m a b . MonadIO m => Int -> [ThreadPoolEntry m a b] -> m ()
runParallel _ [] = return ()
runParallel n entries =
    do when (n < 1) $ fail ("invalid number of workers: " ++ show n)
       fromWorker <- liftIO $ newNamedChan "fromWorker"
       let nWorkers = min n (length entries)
       toWorkers <- mapM (\i -> liftIO $ mkWorker i fromWorker) [1..nWorkers]
       let (initEntries, restEntries) = splitAt nWorkers entries
       mapM_ (\(mvar, entry) -> runEntry entry mvar) (zip toWorkers initEntries)
       loop fromWorker nWorkers restEntries
    where
      loop :: FromWorker m b -> Int -> [ThreadPoolEntry m a b] -> m ()
      loop fromWorker nWorkers [] =
          cleanup fromWorker nWorkers
      loop fromWorker nWorkers (x:xs) =
          do toWorker <- waitForWorkerResult fromWorker
             runEntry x toWorker
             loop fromWorker nWorkers xs
      cleanup :: FromWorker m b -> Int -> m ()
      -- n is the number of workers that will still write to fromWorker
      cleanup fromWorker n =
          do debug ("cleanup, n=" ++ show n)
             toWorker <- waitForWorkerResult fromWorker
             liftIO $ putNamedMVar toWorker Done
             when (n > 1) $ cleanup fromWorker (n - 1)
      waitForWorkerResult :: FromWorker m b -> m (ToWorker m b)
      waitForWorkerResult fromWorker =
          do WorkResult postAction toWorker <- liftIO $ readNamedChan fromWorker
             postAction
             return toWorker
      runEntry :: ThreadPoolEntry m a b -> ToWorker m b -> m ()
      runEntry (pre, action, post) toWorker =
          do a <- pre
             liftIO $ putNamedMVar toWorker (Work (action a) post)
      mkWorker :: Int -> FromWorker m b -> IO (ToWorker m b)
      mkWorker i fromWorker =
          do toWorker <- newEmptyNamedMVar ("worker" ++ show i)
             let loop = do workItem <- takeNamedMVar toWorker
                           case workItem of
                             Done ->
                                 do debug ("worker" ++ show i ++ " exiting!")
                                    return ()
                             Work action post ->
                                 do res <- Ex.try action
                                    _ <- Ex.evaluate res
                                    writeNamedChan fromWorker (WorkResult (post res) toWorker)
                                    loop
             _ <- forkIO (loop `Ex.catch` (\(e::Ex.BlockedIndefinitelyOnMVar) ->
                                          fail ("worker " ++ show i ++ ": " ++ show e)))
             return toWorker

--
-- Debugging and testing
--

_DEBUG_ = False

newNamedChan :: String -> IO (NamedChan a)
newNamedChan name =
    do chan <- newChan
       return (name, chan)

readNamedChan :: Show a => NamedChan a -> IO a
readNamedChan (name, chan) =
    do debug ("readChan[" ++ name ++ "]...")
       x <- readChan chan
       debug ("DONE readChan[" ++ name ++"]=" ++ show x)
       return x

writeNamedChan :: Show a => NamedChan a -> a -> IO ()
writeNamedChan (name, chan) x =
    do debug ("writeChan[" ++ name ++ "]=" ++ show x)
       writeChan chan x

newEmptyNamedMVar :: String -> IO (NamedMVar a)
newEmptyNamedMVar name =
    do mvar <- newEmptyMVar
       return (name, mvar)

putNamedMVar :: Show a => NamedMVar a -> a -> IO ()
putNamedMVar (name, mvar) x =
    do debug ("putMVar[" ++ name ++ "]=" ++ show x ++ "...")
       putMVar mvar x
       debug ("DONE putMVar[" ++ name ++ "]=" ++ show x)

takeNamedMVar :: Show a => NamedMVar a -> IO a
takeNamedMVar (name, mvar) =
    do debug ("takeMVar[" ++ name ++ "]...")
       x <- takeMVar mvar
       debug ("DONE takeMVar[" ++ name ++ "]=" ++ show x)
       return x

debug :: MonadIO m => String -> m ()
debug s = if _DEBUG_ then liftIO $ putStrLn s else return ()

runTestParallel :: Int -> Int -> IO ()
runTestParallel nEntries n =
    do putStrLn ("Running test " ++ show n)
       boxes <- mapM (\i -> do mvar <- newEmptyNamedMVar ("testbox" ++ show i)
                               return (mvar, i))
                      [1..nEntries]
       let entries = map mkEntry boxes
       runParallel n entries
       debug ("Checking boxes in test " ++ show n)
       --runSequentially entries
       mapM_ assertBox boxes
       putStrLn ("Test " ++ show n ++ " successful")
    where
      mkEntry (mvar, i) =
          let pre = myThreadId
              post x = case x of
                         Left err -> fail ("Exception in worker thread: " ++ show err)
                         Right y -> do tid <- myThreadId
                                       putNamedMVar mvar (y, tid)
              action x = do tid <- myThreadId
                            j <- randomIO
                            let micros = (j `mod` 50)
                            threadDelay micros
                            return (x, tid, i)
          in (pre, action, post)
      assertBox (mvar, i) =
         do ((preTid, actionTid, i'), postTid) <- takeNamedMVar mvar
            tid <- myThreadId
            assertEq "pre-tid" tid preTid
            assertEq "post-tid" tid postTid
            assertNeq "action-tid" tid actionTid
            assertEq "i" i i'
      assertEq what exp act =
          when (exp /= act) $ fail (what ++ " wrong, expected=" ++ show exp ++ ", actual=" ++
                                    show act)
      assertNeq what exp act =
          when (exp == act) $ fail (what ++ " wrong, did not expected " ++ show exp)

threadPoolTest (i, j) nEntries =
    mapM (runTestParallel nEntries) [i..j] `Ex.catch`
             (\(e::Ex.BlockedIndefinitelyOnMVar) ->
                  fail ("main-thread blocked " ++ show e))
