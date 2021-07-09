# `monad-metrics`

[![Build Status](https://travis-ci.org/parsonsmatt/monad-metrics.svg?branch=master)](https://travis-ci.org/parsonsmatt/monad-metrics)

This library defines a convenient wrapper and API for using [EKG][] metrics in
your application. It's heavily inspired by the metrics code that Taylor Fausak
used in his Haskell application [blunt](https://github.com/tfausak/blunt).

# Usage

This [README is an executable literate Haskell
file](https://github.com/silky/literate-readme). If you have [stack][] installed, then you can run the file with:

```
./README.lhs
```

We'll need to start with the import/pragma boilerplate:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Control.Monad.Metrics as Metrics
import           Control.Monad.Metrics (Metrics, Resolution(..), MonadMetrics(..))
import           Control.Monad.Reader
import qualified System.Metrics        as EKG
```

The `Control.Monad.Metrics` module is designed to be imported qualified.

### Initialize!

First, you need to initialize the `Metrics` data type. You can do so using
`initialize` (to create a new EKG store) or `initializeWith` if you want to
pass a preexisting store.

```haskell
initializing :: Bool -> EKG.Store -> IO Metrics
initializing True store = Metrics.initializeWith store
initializing False _    = Metrics.initialize
```

### Embed!

The next step is to implement an instance of the class `MonadMetrics` for your
monad transformer stack. This library has explicitly decided not to provide a
concrete monad transformer to reduce the dependency footprint. Fortunately,
it's pretty easy!

Suppose you've got the following stack:

```haskell
type App = ReaderT Config IO

data Config = Config { configMetrics :: Metrics }
```

then you can easily get the required instance with:

```haskell
instance MonadMetrics (ReaderT Config IO) where
    getMetrics = asks configMetrics
```

Now, you're off to the races! Let's record some metrics.

If you're after a really simple embedding, you can use `run` or `run'`:

```haskell
simple :: Int -> IO ()
simple i = 
    Metrics.run $ do
        metrics <- Metrics.getMetrics
        Metrics.gauge "Simple" i
        forM_ [1..i] $ \_ -> do
            Metrics.increment "Count!"

gettingThere :: IO ()
gettingThere = 
    Metrics.run' (\metrics -> Config metrics) $ do
        liftIO $ putStrLn "it accepts a constructor"
```

### Measure!

Once your application has the required instance, you can use [EKG][]'s metrics
(counters, gauges, labels, distributions). 

For detailed descriptions of the various metric types, see the corresponding [EKG][] documentation:

- [Counter][]
- [Distribution][]
- [Gauge][]
- [Label][]

Generally, the library provides "sane default" functions which accept the name
of the metric to work with and the value to contribute to that metric.

```haskell
w = Metrics.label "Foo" "Bar"
x = Metrics.counter "MetricName" 6
y = Metrics.distribution "Distribute" 3.4
z = Metrics.gauge "Gauge" 7
```

Generalized versions of these functions are available with an apostrophe. Labels accept any `Show`able value, while gauges and counters accept any `Integral` value.

```haskell
a = Metrics.label' "List" [1,2,3]
b = Metrics.counter' "Count" (3 :: Integer)
```

#### Timers

You can time actions with `timed`, which has a resolution of seconds. You can
use `timed'` which accepts a `Resolution` argument to provide a different
scale.

```haskell
timedProcess :: App Int
timedProcess = 
    Metrics.timed "summing1" $ do
        pure $! sum [1 .. 100000]

timedInMilliseconds :: App Int
timedInMilliseconds = 
    Metrics.timed' Microseconds "summing2" $ do
        pure $! sum [1..100]
```

# A demonstration

```haskell
main :: IO ()
main = do
    metrics <- Metrics.initialize
    flip runReaderT (Config metrics) $ do
        Metrics.label "ProgramName" "README"
        forM_ [1..10] $ \_ -> do
            Metrics.increment "up-to-ten"
        Metrics.timed' Nanoseconds "Whatever" $ do
            liftIO $ putStrLn "Hello World!" 
```

[EKG]: http://hackage.haskell.org/package/ekg-core
[stack]: https://www.haskellstack.org/
[Counter]: http://hackage.haskell.org/package/ekg-core-0.1.1.1/docs/System-Metrics-Counter.html
[Gauge]:  http://hackage.haskell.org/package/ekg-core-0.1.1.1/docs/System-Metrics-Gauge.html
[Distribution]: http://hackage.haskell.org/package/ekg-core-0.1.1.1/docs/System-Metrics-Distribution.html
[Label]: http://hackage.haskell.org/package/ekg-core-0.1.1.1/docs/System-Metrics-Label.html
