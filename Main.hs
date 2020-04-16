{-# language RecursiveDo, TypeFamilies, FlexibleContexts, BangPatterns, TupleSections, LambdaCase, GeneralizedNewtypeDeriving #-}
module Main where

import Prelude hiding (filter)

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad

import Data.Coerce (coerce)

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, readMVar, swapMVar)

import Data.Time.Clock.System (getSystemTime, SystemTime(..))
import Data.Time.Clock (getCurrentTime, UTCTime(..), DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime, diffUTCTime)

import Data.Sequence (Seq(..), (|>), (<|) )
import qualified Data.Sequence as Seq

import Data.Functor ((<&>), ($>))
import Data.Function ((&))

import Control.Arrow ((>>>))

import Data.List (sortOn, foldl')

import Data.Witherable (filter)

import Data.Either (isLeft, isRight)
import Data.Either.Combinators (mapLeft)

import Reflex.Host.Basic
import Reflex


picosToMicros :: Integer -> Int 
picosToMicros picos = 
  let
    (micros, remainingPicos) = picos `divMod` (10^6)
  in
    if remainingPicos > 0 then fromIntegral micros + 1 else fromIntegral micros

newtype Micros = Micros { unMicros :: Int } deriving (Show, Eq)
newtype Picos = Picos { unPicos :: Integer } deriving (Show, Eq, Ord, Num)

getPicosAfterMidnight :: IO Picos
getPicosAfterMidnight = getCurrentTime <&> (utctDayTime >>> diffTimeToPicoseconds >>> toInteger >>> coerce)

-- Wait until next interval start where we start counting intervals from midnight.
-- Returns picos elapsed since before threadDelay
waitUntilNext :: Picos -> IO Picos
waitUntilNext intervalLength = do
  picosAfterMidnightBefore <- getPicosAfterMidnight

  let
    picosAfterPrev = unPicos picosAfterMidnightBefore `mod` unPicos intervalLength
    microsUntilNext = picosToMicros (coerce intervalLength - picosAfterPrev)

  threadDelay microsUntilNext
  picosAfterMidnightAfter <- getPicosAfterMidnight
  return $ Picos (unPicos picosAfterMidnightAfter - unPicos picosAfterMidnightBefore)

-- Emit events at the start of every interval starting at the start of the next
-- interval. Intervals start counting from midnight. Each tick event contains
-- the Picos elapsed since the last tick.
-- Returns a tick event and accompanying mvar used to change interval length.
tick :: (PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadIO m) 
     => Picos -- Picos between ticks
     -> m (Event t Picos, MVar Picos)
tick intervalLength = do
  mvar <- newMVar intervalLength & liftIO
  ePostBuild <- getPostBuild

  let
    go fire = do
      let 
        loop = do
          elapsed <- readMVar mvar >>= waitUntilNext
          fire elapsed
          loop
      loop

  eTick <- performEventAsync $ liftIO . void . forkIO . go <$ ePostBuild
  return (eTick, mvar)

countE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t Int)
countE e = mapAccum_ (\a b -> let a' = a + 1 in (a', a')) 0 e

newtype Job e a = Job (IO (Either e a))

-- Run jobs asynchronously, but return the results in FIFO order. Failed jobs
-- are paired with their error result.
runJobsAsync :: Seq (Job e a) -> (Either (e, Job e a) a -> IO ()) -> IO ()
runJobsAsync jobs callback = forM_ jobs $ \job@(Job io) ->
  do
    mvar <- newEmptyMVar
    forkIO (io <&> mapLeft (,job) >>= putMVar mvar)
    takeMVar mvar >>= callback

-- The initial rate's interval length is expected to be smaller than every
-- rate constraint's interval length
getConstrainedRate :: (Int, Picos) -> Seq (Int, Picos) -> (Int, Picos)
getConstrainedRate initialRate rateConstraints =
  ((initialRate <| rateConstraints) <&> fst & minimum, snd initialRate)

-- Subtract the incoming rate from the rate constraints. If a particular
-- constraints interval length becomes less than 0 then reset the
-- constraint to what it was initially.
updateRateConstraints :: Seq (Int, Picos) -> (Int, Picos) -> Seq (Int, Picos) -> Seq (Int, Picos)
updateRateConstraints initialRateConstraints (incomingJobsPerTick, incomingIntervalLength) currRateConstraints =
  foldl' (\updatedRateConstraints (initialRateConstraint, (jobsPerTickConstraint, intervalLengthConstraint)) ->
    let
      updatedJobsPerTick = jobsPerTickConstraint - incomingJobsPerTick
      updatedIntervalLength = intervalLengthConstraint - incomingIntervalLength
      updatedRateConstraint =
        if updatedIntervalLength <= 0 
        then initialRateConstraint 
        else (updatedJobsPerTick, updatedIntervalLength)
    in
      updatedRateConstraints |> updatedRateConstraint
  ) Seq.empty (Seq.zip initialRateConstraints currRateConstraints)

-- Batches incoming jobs according to given initial rate and rate constraints,
-- then executes them asynchronously. Depending on the job results the given
-- job governor function is used to adjust the rate.
rateLimit :: ( Reflex t
             , PostBuild t m
             , MonadHold t m
             , MonadFix m
             , PerformEvent t m
             , MonadIO (Performable m)
             , TriggerEvent t m
             , Adjustable t m
             , MonadIO m
              )
          => (Int, Picos) -- initial job rate - interval length must be smaller than any job rate constraint
          -> Seq (Int, Picos) -- job rate constraints
          -> ((Int, Picos) -> Either e a -> (Int, Picos)) -- job rate governor
          -> Event t (Job e a) -- job events
          -> m (Event t a)
rateLimit initialRate initialRateConstraints rateGovernor eJob = mdo
  (eTick, mvarTickIntervalLength) <- tick (initialRate & snd)
  
  -- we update the interval length one tick after updating jobs per tick
  bRateConstraints <- 
    let
      mbRateConstraints = 
        accumB (\(prevRateConstraints, prevIntervalLength) (jobsPerTick, intervalLength) ->
          (updateRateConstraints initialRateConstraints (jobsPerTick, prevIntervalLength) prevRateConstraints, intervalLength)
        ) (initialRateConstraints, 0) eTickRate
    in
      mbRateConstraints <&> (<&> fst)

  bRate <-
    let
      eRate = attachWith (flip getConstrainedRate) bRateConstraints eRateUpdate
    in
      hold initialRate eRate

  let 
    eRateUpdate = attachWith rateGovernor bRate (eJobResult <&> mapLeft fst)
    eTickRate = tag bRate eTick
    eAddNewJob = eJob <&> (\job -> (|> job))
    eAddRetryJob = eJobRetry <&> (\job -> (job <|))
    eJobsPerTick = eTickRate <&> fst
    eDropJobs = eJobsPerTick <&> Seq.drop
    eTakeJobs = eJobsPerTick <&> Seq.take

  bJobs <- accumB (&) Seq.empty (mergeWith (.) [eAddRetryJob, eAddNewJob, eDropJobs])

  let
    eChosenJobs = attachWith (&) bJobs eTakeJobs

  -- tracing
  performEvent_ $ tag bRateConstraints eTick <&> (\rcs -> print rcs & liftIO)
  performEvent_ $ eTickRate <&> (liftIO . print)
  performEvent_ $ (\jobs -> liftIO $ putStrLn ("BJob Count: " ++ show (length jobs) )) <$> tag bJobs eTickRate
  performEvent_ $ (\activeJobs -> liftIO $ putStrLn ("CJob Count: " ++ show (length activeJobs) )) <$> eChosenJobs

  -- update tick interval length
  performEvent_ $ eRateUpdate <&> (\(_, intervalLength) -> swapMVar mvarTickIntervalLength intervalLength & void & liftIO)
  -- run jobs
  eJobResult <- eChosenJobs <&> (\jobs -> liftIO . (runJobsAsync jobs)) & performEventAsync

  let
    (eJobFailure, eJobSuccess) = eJobResult & fanEither
    eJobRetry = eJobFailure <&> snd
  
  return eJobSuccess

-- BasicGuest is a concrete type that implements a Reflex Host and all the type classes we need
guest :: (BasicGuestConstraints t m, Reflex t) => BasicGuest t m ()
guest =  mdo
  (eJobTick, jobTickIntervalLengthMvar) <- tick (Picos $ 10^12 `div` 100)

  eJobTickCount <- countE eJobTick
  let 
    eJob = eJobTickCount <&> (\count -> Job $ Left <$> putStrLn ("Job " ++ show count))

  rateLimit (20, Picos $ 10^12) (Seq.fromList [(15000, 36*10^14)]) (\(jobsPerTick, intervalLength) _ -> (jobsPerTick + 1, intervalLength)) eJob

  pure ()

main :: IO ()
main = do
  basicHostForever $ guest 
