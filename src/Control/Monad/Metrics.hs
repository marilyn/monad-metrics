{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Control.Monad.Metrics
Description : An easy interface to recording metrics.
Copyright   : (c) Matt Parsons, 2017
                  Taylor Fausak, 2016
License     : MIT
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module presents an easy interface that you can use to collect metrics
about your application.  It uses EKG from "System.Metrics" under the hood
and is inspired by Taylor Fausak's <https://github.com/tfausak/blunt blunt>
application.

This module is designed to be imported qualified.
-}
module Control.Monad.Metrics
    ( -- * The Type Class
      MonadMetrics(..)
      -- * Initializing
      -- $initializing
    , initialize
    , initializeWith
    , run
    , run'
      -- * Collecting Metrics
      -- $collecting
    , increment
    , counter
    , counter'
    , gauge
    , gauge'
    , distribution
    , timed
    , timed'
    , timedList
    , label
    , label'
    , Resolution(..)
    -- * The Metrics Type
    -- $metrictype
    , Metrics
    , metricsCounters
    , metricsGauges
    , metricsLabels
    , metricsStore
    ) where

import           Control.Monad                  (liftM, forM_)
import           Control.Monad.Catch            (MonadMask, bracket)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (MonadReader (..), ReaderT (..))
import           Control.Monad.Trans            (MonadTrans (..))
import           Data.Hashable                  (Hashable)
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HashMap
import           Data.IORef                     (IORef, atomicModifyIORef',
                                                 newIORef)
import           Data.Monoid                    (mempty)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           System.Clock                   (Clock (..), getTime)
import           System.IO.Unsafe               (unsafeInterleaveIO)
import qualified System.Metrics                 as EKG
import           System.Metrics.Counter         as Counter
import           System.Metrics.Distribution    as Distribution
import           System.Metrics.Gauge           as Gauge
import           System.Metrics.Label           as Label

import           Prelude

import           Control.Monad.Metrics.Internal

-- | A type can be an instance of 'MonadMetrics' if it can provide a 'Metrics'
-- somehow. Commonly, this will be implemented as a 'ReaderT' where some
-- field in the environment is the 'Metrics' data.
--
-- * /Since v0.1.0.0/
class Monad m => MonadMetrics m where
    getMetrics :: m Metrics

instance {-# OVERLAPPABLE #-} (MonadMetrics m, MonadTrans t, Monad (t m)) => MonadMetrics (t m) where
    getMetrics = lift getMetrics

instance Monad m => MonadMetrics (ReaderT Metrics m) where
    getMetrics = ask

-- $initializing
-- This library tends to provide simple functions with plain names and
-- generalized functions with apostrophes. When initializing the metrics,
-- you can use 'initialize' if you don't need fine control over the store,
-- or you can use 'initializeWith' if your application already has a store
-- that it uses.
--
-- Likewise, we provide 'run' for the simplest case, and 'run'' for the
-- more complex case where you have some larger type.
--
-- The most flexible way to use the library is to implement the
-- 'MonadMetrics' class.

-- | Enhances the base monad with metrics. This works for very simple
-- cases, where you don't have a 'Reader' involved yet. If your stack
-- already has a 'Reader', then you'll get some annoying type problems with
-- this. Switch over to 'run'', or alternatively, define your own
-- 'MonadMetrics' instance.
--
-- */Since v0.1.0.0/
run :: MonadIO m => ReaderT Metrics m a -> m a
run = run' id

-- | Adds metric recording capabilities to the given action. The first
-- parameter is a function which accepts a 'Metrics' value and creates the
-- final @r@ value to be used in the action. This is useful when you have
-- a preexisting 'ReaderT' in your stack, and you want to enhance it with
-- metrics.
--
-- @
-- data Config = Config { size :: Int, metrics' :: Metrics }
--
-- main = 'runWithMetrics' (Config 10) $ do
--     num <- asks size
--     forM_ [1 .. size] \_ -> Metrics.increment "foo"
-- @
--
-- */Since v0.1.0.0/
run' :: MonadIO m => (Metrics -> r) -> ReaderT r m a -> m a
run' k action = do
    m <- liftIO initialize
    runReaderT action (k m)

-- | Initializes a 'Metrics' value with the given 'System.Metrics.Store'.
--
-- */Since v0.1.0.0/
initializeWith :: EKG.Store -> IO Metrics
initializeWith _metricsStore = do
    _metricsCounters <- newIORef mempty
    _metricsDistributions <- newIORef mempty
    _metricsGauges <- newIORef mempty
    _metricsLabels <- newIORef mempty
    return Metrics{..}

-- | Initializes a 'Metrics' value, creating a new 'System.Metrics.Store'
-- for it.
--
-- * /Since v0.1.0.0/
initialize :: IO Metrics
initialize = EKG.newStore >>= initializeWith

-- $collecting
-- As with initialization, the library provides "common case" functions
-- with a plain name and generalized functions with an apostrophe.
--
-- * 'increment', 'counter', 'counter''
-- * 'gauge', 'gauge''
-- * 'timed', 'timed''
-- * 'label', 'label''
--
-- Only 'distribution' isn't generalized.

-- | Increment the named counter by 1.
--
-- * /Since v0.1.0.0/
increment :: (MonadIO m, MonadMetrics m) => Text -> m ()
increment name = counter name 1

-- | Adds the value to the named 'System.Metrics.Counter.Counter'.
--
-- * /Since v0.1.0.0/
counter' :: (MonadIO m, MonadMetrics m, Integral int) => Text -> int -> m ()
counter' =
    modifyMetric Counter.add fromIntegral EKG.createCounter _metricsCounters

-- | A type specialized version of 'counter'' to avoid ambiguous type
-- errors.
--
-- * /Since v0.1.0.0/
counter :: (MonadIO m, MonadMetrics m) => Text -> Int -> m ()
counter = counter'

-- | Add the value to the named 'System.Metrics.Distribution.Distribution'.
--
-- * /Since v0.1.0.0/
distribution :: (MonadIO m, MonadMetrics m) => Text -> Double -> m ()
distribution =
    modifyMetric Distribution.add id EKG.createDistribution _metricsDistributions

-- | Set the value of the named 'System.Metrics.Distribution.Gauge'.
--
-- * /Since v0.1.0.0/
gauge' :: (MonadIO m, MonadMetrics m, Integral int) => Text -> int -> m ()
gauge' =
    modifyMetric Gauge.set fromIntegral EKG.createGauge _metricsGauges

-- | A type specialized version of 'gauge'' to avoid ambiguous types.
--
-- * /Since v0.1.0.0/
gauge :: (MonadIO m, MonadMetrics m) => Text -> Int -> m ()
gauge = gauge'

-- | Record the time taken to perform the named action. The number is
-- stored in a 'System.Metrics.Disribution.Distribution' and is converted
-- to the specified 'Resolution'.
--
-- * /Since v0.1.0.0/
timed' :: (MonadIO m, MonadMetrics m, MonadMask m) => Resolution -> Text -> m a -> m a
timed' resolution name action = timedList resolution [name] action

-- | Record the time taken to perform the action, under several names at once.
-- The number is stored in a 'System.Metrics.Disribution.Distribution' and is
-- converted to the specified 'Resolution'.
--
-- This is useful to store the same durations data sectioned by different criteria, e.g.:
--
-- @
-- timedList Seconds ["request.byUser." <> userName, "request.byType." <> requestType] $ do
--     ...
-- @
--
-- So you will have @"request.byUser.someuser"@ storing duration distribution for requests
-- of user @"someuser"@ of any type; and @"request.byType.sometype"@ storing
-- duration distribution for requests of type @"sometype"@ from any user.
--
timedList :: (MonadIO m, MonadMetrics m, MonadMask m) => Resolution -> [Text] -> m a -> m a
timedList resolution names action = 
    bracket (liftIO (getTime Monotonic)) finish (const action)
  where
    finish start = do
        end <- liftIO (getTime Monotonic)
        forM_ names $ \name ->
            distribution name (diffTime resolution end start)

-- | Record the time of executing the given action in seconds. Defers to
-- 'timed''.
--
-- * /Since v0.1.0.0/
timed :: (MonadIO m, MonadMetrics m, MonadMask m) => Text -> m a -> m a
timed = timed' Seconds

-- | Set the 'Label' to the given 'Text' value.
--
-- * /Since v0.1.0.0/
label :: (MonadIO m, MonadMetrics m) => Text -> Text -> m ()
label = modifyMetric Label.set id EKG.createLabel _metricsLabels

-- | Set the 'Label' to the 'Show'n value of whatever you pass in.
--
-- * /Since v0.1.0.0/
label' :: (MonadIO m, MonadMetrics m, Show a) => Text -> a -> m ()
label' l = label l . Text.pack . show

-- $metrictype
-- The 'Metric' type contains an 'IORef' to a 'HashMap' from 'Text' labels to
-- the various counters, and a 'EKG.Store' to register them with. If you
-- must use the 'Metric' value directly, then you are recommended to use
-- the lenses provided for compatibility.

-------------------------------------------------------------------------------

modifyMetric
    :: (MonadMetrics m, MonadIO m)
    => (t -> t1 -> IO b) -- ^ The action to add a value to a metric.
    -> (t2 -> t1) -- ^ A conversion function from input to metric value.
    -> (Text -> EKG.Store -> IO t) -- ^ The function for creating a new metric.
    -> (Metrics -> IORef (HashMap Text t)) -- ^ A way of getting the current metrics.
    -> Text -- ^ The name of the metric to use.
    -> t2 -- ^ The value the end user can provide.
    -> m b
modifyMetric adder converter creator getter name value = do
    bar <- lookupOrCreate getter creator name
    liftIO $ adder bar (converter value)

lookupOrCreate
    :: (MonadMetrics m, MonadIO m, Eq k, Hashable k)
    => (Metrics -> IORef (HashMap k a)) -> (k -> EKG.Store -> IO a) -> k -> m a
lookupOrCreate getter creator name = do
    ref <- liftM getter getMetrics
    -- unsafeInterleaveIO is used here to defer creating the metric into
    -- the 'atomicModifyIORef'' function. We lazily create the value here,
    -- and the resulting IO action only gets run to create the metric when
    -- the named metric is not present in the map.
    newMetric <- liftIO . unsafeInterleaveIO . creator name =<< liftM _metricsStore getMetrics
    liftIO $ atomicModifyIORef' ref (\container ->
        case HashMap.lookup name container of
            Nothing ->
                (HashMap.insert name newMetric container, newMetric)
            Just metric ->
                (container, metric))
