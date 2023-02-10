{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

#if __GLASGOW_HASKELL__ <904
#define OPAQUE NOINLINE
#endif

-- | This module is adapted fork of @Test.Tasty.Bench@ from @tasty-bench@
-- package. Check its documentation.
--
-- This module has (almost) the same API but uses @libperf@ to measure instruction counts.
-- They are more stable performance metric, which is useful while still
-- working on the problem.
--
-- The module is named the same so you can easily switch from using
-- tasty-bench to tasty-perfbench by only switching a @build-depends@
-- in your @.cabal@ file.
--
-- The 'Benchmarkable' is run /only once/.
-- There will be about 4000-5000 additional instructions counted in.
--
-- This module isn't great for "nanobenchmarks".
-- The reason to measure only once is to have proper values for branch misspredictions and cache misses. Their counts aren't linear in iterations.
-- In other words: measurements are made cold. If you need hot measurements, make the 'Benchmarkable' do more work.
--
-- You may include
--
-- @
-- , 'bench' "id ()"     $ 'whnf' id ()
-- , 'bench' "Just True" $ 'nf' Just True
-- @
--
-- in your benchmark suite to measure the "does (almost) nothing" operation(s) on your system to get an idea of what is the base level.
-- (On my machine both use 5.6K instructions).
--
-- Using performance monitoring functionality requires additional privileges.
-- Probably easiest way is to:
--
-- @
-- sudo sh -c 'echo 1 | tee \/proc\/sys\/kernel\/perf_event_paranoid'
-- @
--
-- which allows per-process performance monitoring only and excludes system wide performance monitoring.
-- That is enough for @tasty-perfbench@ to do its job.
-- (@libperf@ also counts events happening in kernel space).
-- See https://www.kernel.org/doc/html/latest/admin-guide/perf-security.html
-- for more information.
--
module Test.Tasty.Bench
  (
  -- * Running 'Benchmark'
    defaultMain
  , Benchmark
  , bench
  , bgroup
  , bcompare
  , bcompareWithin
  , env
  , envWithCleanup
  -- * Creating 'Benchmarkable'
  , Benchmarkable(..)
  , nf
  , whnf
  , nfIO
  , whnfIO
  , nfAppIO
  , whnfAppIO
  -- * Ingredients
  , benchIngredients
  , consoleBenchReporter
  , csvReporter
  , FailIfSlower(..)
  , FailIfFaster(..)
  , BenchMode(..)
  -- * Utils
  , locateBenchmark
  , mapLeafBenchmarks
  ) where

import Prelude hiding (Int, Integer)
import qualified Prelude
import Control.Applicative
import Control.Arrow (first, second)
import Control.DeepSeq (NFData, force)
import Control.Exception (bracket, evaluate)
import Control.Monad (void, unless, guard, (>=>), when)
import Data.Data (Typeable)
import Data.Foldable (foldMap, traverse_)
import Data.Int (Int64)
import Data.List (intercalate, stripPrefix, isPrefixOf, genericLength, genericDrop, foldl1')
import Data.Maybe (fromMaybe)
import Data.Monoid (All(..), Any(..))
import Data.Proxy ( Proxy(..) )
import Data.Traversable (forM)
import Data.Word (Word64)
import GHC.Conc
import GHC.Stats (RTSStats, gcs, major_gcs, getRTSStats, getRTSStatsEnabled)
import GHC.IO.Encoding ( utf8, setLocaleEncoding )
import System.Exit ( exitFailure )
import System.IO ( Handle, utf8, hClose, hSetBuffering, hPutStrLn, openFile, stderr, BufferMode(LineBuffering), IOMode(WriteMode) )
import System.IO.Unsafe ( unsafePerformIO )
import System.Mem ( performGC )
import Text.Printf ( printf )
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Map as M

import Test.Tasty hiding (defaultMain)
import qualified Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Options
import Test.Tasty.Patterns.Eval (eval, asB, withFields)
import Test.Tasty.Patterns.Types (Expr (And, Field, IntLit, NF, StringLit, Sub))
import qualified Test.Tasty.Patterns.Types as Patterns
import Test.Tasty.Providers
    ( IsTest(..), Result, singleTest, testFailed, testPassed )
import Test.Tasty.Runners

import System.IO.CodePage (withCP65001)

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif

#ifdef MIN_VERSION_libperf
import qualified LibPerf
#endif

data R a = R
    { rInstructions       :: a
    , rBranchInstructions :: a
    , rBranchMisses       :: a
    , rCacheReferences    :: a
    , rCacheMisses        :: a
    }
  deriving (Functor, Foldable, Traversable)

instance Applicative R where
    pure x = R x x x x x

    R f1 f2 f3 f4 f5 <*> R x1 x2 x3 x4 x5 =
        R (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)

#ifdef MIN_VERSION_libperf
perfGroupHandle :: LibPerf.PerfGroupHandle R
perfGroupHandle = unsafePerformIO $
    LibPerf.perfGroupOpen (R LibPerf.HwInstructions LibPerf.HwBranchInstructions LibPerf.HwBranchMisses LibPerf.HwCacheReferences LibPerf.HwCacheMisses)
{-# OPAQUE perfGroupHandle #-}
#endif

data BenchMode = Instructions -- ^ Measure CPU time.

  deriving (Typeable)

-- | In addition to @--fail-if-slower@ command-line option,
-- one can adjust an upper bound of acceptable slow down
-- in comparison to baseline for
-- individual benchmarks and groups of benchmarks
-- using 'adjustOption' and 'localOption'.
--
-- E. g., set upper bound of acceptable slow down to 10% as follows:
--
-- > import Test.Tasty (localOption)
-- > localOption (FailIfSlower 0.10) (bgroup [...])
newtype FailIfSlower = FailIfSlower Double
  deriving (Show, Read, Typeable)

instance IsOption FailIfSlower where
  defaultValue = FailIfSlower (1.0 / 0.0)
  parseValue = fmap FailIfSlower . parsePositivePercents
  optionName = pure "fail-if-slower"
  optionHelp = pure "Upper bound of acceptable slow down in percents. If a benchmark is unacceptably slower than baseline (see --baseline), it will be reported as failed."

-- | In addition to @--fail-if-faster@ command-line option,
-- one can adjust an upper bound of acceptable speed up
-- in comparison to baseline for
-- individual benchmarks and groups of benchmarks
-- using 'adjustOption' and 'localOption'.
--
-- E. g., set upper bound of acceptable speed up to 10% as follows:
--
-- > import Test.Tasty (localOption)
-- > localOption (FailIfFaster 0.10) (bgroup [...])
--
newtype FailIfFaster = FailIfFaster Double
  deriving (Show, Read, Typeable)

instance IsOption FailIfFaster where
  defaultValue = FailIfFaster (1.0 / 0.0)
  parseValue = fmap FailIfFaster . parsePositivePercents
  optionName = pure "fail-if-faster"
  optionHelp = pure "Upper bound of acceptable speed up in percents. If a benchmark is unacceptably faster than baseline (see --baseline), it will be reported as failed."

parsePositivePercents :: String -> Maybe Double
parsePositivePercents xs = do
  x <- safeRead xs
  guard (x > 0)
  pure (x / 100)

instance IsOption BenchMode where
  defaultValue = Instructions
  parseValue v = case v of
    "instructions" -> Just Instructions
    _ -> Nothing
  optionName = pure "perfbench-mode"
  optionHelp = pure "What to perfbench"
  showDefaultValue m = Just $ case m of
    Instructions -> "instructions"

-- | Something that can be benchmarked, produced by 'nf', 'whnf', 'nfIO', 'whnfIO',
-- 'nfAppIO', 'whnfAppIO' below.
--
-- Drop-in replacement for @Criterion.@'Criterion.Benchmarkable' and
-- @Gauge.@'Gauge.Benchmarkable'.
--
newtype Benchmarkable =
    Benchmarkable
  { unBenchmarkable :: IO () -- ^ Run benchmark given number of times.
  } deriving (Typeable)


showCount :: Word64 -> String
showCount i
  | t < 995   = printf "%3.0f   "  t
  | t < 995e1 = printf "%4.2f K"  (t / 1e3)
  | t < 995e2 = printf "%4.1f K"  (t / 1e3)
  | t < 995e3 = printf "%3.0f  K" (t / 1e3)
  | t < 995e4 = printf "%4.2f M"  (t / 1e6)
  | t < 995e5 = printf "%4.1f M"  (t / 1e6)
  | t < 995e6 = printf "%3.0f  M" (t / 1e6)
  | t < 995e7 = printf "%4.2f G"  (t / 1e9)
  | t < 995e8 = printf "%4.1f G"  (t / 1e9)
  | t < 995e9 = printf "%3.0f  G" (t / 1e9)
  | otherwise = printf "%4.3f T"  (t / 1e12)
  where
    t = word64ToDouble i

data Measurement = Measurement
  { measInstructions :: !Word64 -- ^ instructions count
  , measBranchTotal  :: !Word64
  , measBranchMisses :: !Word64
  , measCacheTotal   :: !Word64
  , measCacheMisses  :: !Word64
  , measMajorGC      :: !Word64
  , measMinorGC      :: !Word64
  } deriving (Show, Read)

type Estimate = Measurement

prettyEstimate :: Estimate -> Maybe Estimate -> String
prettyEstimate m ms = intercalate "\n"
    [ line  measInstructions "instructions"
    , line  measBranchTotal "branch instructions"
    , line2 measBranchTotal measBranchMisses "branch misspredictions"
    , line  measCacheTotal "cache references"
    , line2 measCacheTotal measCacheMisses "cache misses"
    , line  measMajorGC "major gcs"
    , line  measMinorGC "major gcs"
    ]
  where
    line f msg =
      let pfx = showCount (f m) ++ " " ++ msg
      in case ms of
        Nothing -> pfx
        Just n -> pfx ++ replicate (40 - length pfx) ' ' ++ printf "  base: %s, change: %+0.1f%%" (showCount y) (percentDiff y x)
          where
            x = f m
            y = f n

    line2 f g msg =
      let pfx = showCount (g m) ++ " " ++ msg ++ printf " (%.01f%%)" (ratio g f)
      in case ms of
        Nothing -> pfx
        Just n -> pfx ++ replicate (40 - length pfx) ' ' ++ printf "  base: %s" (showCount y)
          where
            y = g n

    ratio a b = percent (a m) (b m)
    percent a b = 100 * word64ToDouble a / word64ToDouble (max 1 b)

    percentDiff _ 0 = 0
    percentDiff a b = 100 * (b' - a') / b'
      where
        a' = word64ToDouble a
        b' = word64ToDouble b

data WithLoHi a = WithLoHi
  !a      -- payload
  !Double -- lower bound (e. g., 0.9 for -10% speedup)
  !Double -- upper bound (e. g., 1.2 for +20% slowdown)
  deriving (Show, Read)

measure :: BenchMode -> Benchmarkable -> IO Measurement
#ifdef MIN_VERSION_libperf
measure _benchMode (Benchmarkable act) = do
  -- run once
  -- and to a regression
  -- to cut off as much of framework effect as possible.
  LibPerf.perfGroupReset perfGroupHandle
  performGC
  (majGC, minGC) <- rtsStats
  LibPerf.perfGroupEnable perfGroupHandle
  act
  LibPerf.perfGroupDisable perfGroupHandle
  (majGC', minGC') <- rtsStats
  result <- LibPerf.perfGroupRead perfGroupHandle

  let meas = Measurement
        { measInstructions = rInstructions result
        , measBranchTotal  = rBranchInstructions result
        , measBranchMisses = rBranchMisses result
        , measCacheTotal   = rCacheReferences result
        , measCacheMisses  = rCacheMisses result
        , measMinorGC      = minGC' - minGC
        , measMajorGC      = majGC' - majGC
        }
  pure $! meas
#else
measure _ _ = pure Measurement
        { measInstructions = 0
        , measBranchTotal  = 0
        , measBranchMisses = 0
        , measCacheTotal   = 0
        , measCacheMisses  = 0
        , measMinorGC      = 0
        , measMajorGC      = 0
        }
#endif

rtsStats :: IO (Word64, Word64)
rtsStats = do
    enabled <- getRTSStatsEnabled
    if enabled
    then do
        stats <- getRTSStats
        let majGC = major_gcs stats
        let allGC = gcs stats
        return (fromIntegral majGC, fromIntegral (allGC - majGC))
    else return (0, 0)

instance IsTest Benchmarkable where
  testOptions = pure
    -- FailIfSlower and FailIfFaster must be options of a test provider rather
    -- than options of an ingredient to allow setting them on per-test level.
    [ Option (Proxy :: Proxy FailIfSlower)
    , Option (Proxy :: Proxy FailIfFaster)
    , Option (Proxy :: Proxy BenchMode)
    ]
  run opts b = const $ case getNumThreads (lookupOption opts) of
    1 -> do
      let benchMode = lookupOption opts
      est <- measure benchMode b
      let FailIfSlower ifSlower = lookupOption opts
          FailIfFaster ifFaster = lookupOption opts
      pure $ testPassed $ show (WithLoHi est (1 - ifFaster) (1 + ifSlower))
    _ -> pure $ testFailed "Benchmarks must not be run concurrently. Please pass -j1 and/or avoid +RTS -N."

-- | Attach a name to 'Benchmarkable'.
--
-- This is actually a synonym of 'Test.Tasty.Providers.singleTest' to
-- provide an interface compatible with @Criterion.@'Criterion.bench'
-- and @Gauge.@'Gauge.bench'.
bench :: String -> Benchmarkable -> Benchmark
bench = singleTest

-- | Attach a name to a group of 'Benchmark'.
--
-- This is actually a synonym of 'Test.Tasty.testGroup' to provide an
-- interface compatible with @Criterion.@'Criterion.bgroup' and
-- @Gauge@.'Gauge.bgroup'.
bgroup :: String -> [Benchmark] -> Benchmark
bgroup = testGroup

-- | Compare benchmarks, reporting relative speed up or slow down.
--
-- This function is a vague reminiscence of @bcompare@, which existed in pre-1.0
-- versions of @criterion@, but their types are incompatible. Under the hood
-- 'bcompare' is a thin wrapper over 'after' and requires @tasty-1.2@.
-- If you use 'bcompare', it is prudent to add @tasty >= 1.2@ to @build-depends@
-- section of your cabal file.
--
-- Here is a basic example:
--
-- > import Test.Tasty.Bench
-- >
-- > fibo :: Int -> Integer
-- > fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)
-- >
-- > main :: IO ()
-- > main = defaultMain
-- >   [ bgroup "fibonacci numbers"
-- >     [ bcompare "tenth"  $ bench "fifth"     $ nf fibo  5
-- >     ,                     bench "tenth"     $ nf fibo 10
-- >     , bcompare "tenth"  $ bench "twentieth" $ nf fibo 20
-- >     ]
-- >   ]
--
-- More complex examples:
--
-- * https://hackage.haskell.org/package/chimera-0.3.2.0/src/bench/Bench.hs
-- * https://hackage.haskell.org/package/fast-digits-0.3.1.0/src/bench/Bench.hs
-- * https://hackage.haskell.org/package/unicode-data-0.3.0/src/bench/Main.hs
bcompare
  :: String
  -- ^ @tasty@ pattern, which must unambiguously
  -- match a unique baseline benchmark. Consider using 'locateBenchmark' to construct it.
  -> Benchmark
  -- ^ Benchmark (or a group of benchmarks)
  -- to be compared against the baseline benchmark by dividing measured mean times.
  -- The result is reported by 'consoleBenchReporter', e. g., 0.50x or 1.25x.
  -> Benchmark
bcompare = bcompareWithin (-1/0) (1/0)

-- | Same as 'bcompare', but takes expected lower and upper bounds of
-- comparison. If the result is not within provided bounds, benchmark fails.
-- This allows to create portable performance tests: instead of comparing
-- to an absolute timeout or to previous runs, you can state that one implementation
-- of an algorithm must be faster than another.
--
-- E. g., 'bcompareWithin' 2.0 3.0 passes only if a benchmark is at least 2x
-- and at most 3x slower than a baseline.
bcompareWithin
  :: Double    -- ^ Lower bound of relative speed up.
  -> Double    -- ^ Upper bound of relative spped up.
  -> String    -- ^ @tasty@ pattern to locate a baseline benchmark.
  -> Benchmark -- ^ Benchmark to compare against baseline.
  -> Benchmark
bcompareWithin lo hi s = case parseExpr s of
  Nothing -> error $ "Could not parse bcompare pattern " ++ s
  Just e  -> after_ AllSucceed (And (StringLit (bcomparePrefix ++ show (lo, hi))) e)

bcomparePrefix :: String
bcomparePrefix = "tasty-perfbench"

-- | Benchmarks are actually just a regular 'Test.Tasty.TestTree' in disguise.
--
-- This is a drop-in replacement for @Criterion.@'Criterion.Benchmark'
-- and @Gauge.@'Gauge.Benchmark'.
type Benchmark = TestTree

-- | Run benchmarks and report results, providing an interface
-- compatible with @Criterion.@'Criterion.defaultMain' and
-- @Gauge.@'Gauge.defaultMain'.
defaultMain :: [Benchmark] -> IO ()
defaultMain bs = do
  let act = Test.Tasty.defaultMainWithIngredients benchIngredients $ testGroup "All" bs
  setLocaleEncoding utf8
  withCP65001 act

-- | List of default benchmark ingredients. This is what 'defaultMain' runs.
benchIngredients :: [Ingredient]
benchIngredients = [listingTests, composeReporters consoleBenchReporter csvReporter]

funcToBench :: (b -> c) -> (a -> b) -> a -> Benchmarkable
funcToBench frc f x = Benchmarkable $ do
  _ <- evaluate (frc (f x))
  pure  ()
{-# OPAQUE funcToBench #-}

-- | 'nf' @f@ @x@ measures time to compute
-- a normal form (by means of 'force') of an application of @f@ to @x@.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentially @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Here is a textbook anti-pattern: 'nf' 'sum' @[1..1000000]@.
-- Since an input list is shared by multiple invocations of 'sum',
-- it will be allocated in memory in full, putting immense pressure
-- on garbage collector. Also no list fusion will happen.
-- A better approach is 'nf' (@\\n@ @->@ 'sum' @[1..n]@) @1000000@.
--
-- If you are measuring an inlinable function,
-- it is prudent to ensure that its invocation is fully saturated,
-- otherwise inlining will not happen. That's why one can often
-- see 'nf' (@\\n@ @->@ @f@ @n@) @x@ instead of 'nf' @f@ @x@.
-- Same applies to rewrite rules.
--
-- While @tasty-bench@ is capable to perform micro- and even nanobenchmarks,
-- such measurements are noisy and involve an overhead. Results are more reliable
-- when @f@ @x@ takes at least several milliseconds.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios (imagine benchmarking 'tail'),
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- Drop-in replacement for @Criterion.@'Criterion.nf' and
-- @Gauge.@'Gauge.nf'.
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = funcToBench force
{-# INLINE nf #-}

-- | 'whnf' @f@ @x@ measures time to compute
-- a weak head normal form of an application of @f@ to @x@.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentially @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Beware that many educational materials contain examples with 'whnf':
-- this is a wrong default.
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nf' instead.
--
-- Here is a textbook anti-pattern: 'whnf' ('Data.List.replicate' @1000000@) @1@.
-- This will succeed in a matter of nanoseconds, because weak head
-- normal form forces only the first element of the list.
--
-- Drop-in replacement for @Criterion.@'Criterion.whnf' and @Gauge.@'Gauge.whnf'.
whnf :: (a -> b) -> a -> Benchmarkable
whnf = funcToBench id
{-# INLINE whnf #-}

ioToBench :: (b -> c) -> IO b -> Benchmarkable
ioToBench frc act = Benchmarkable $ do
    val <- act
    _ <- evaluate (frc val)
    return ()
{-# OPAQUE ioToBench #-}

-- | 'nfIO' @x@ measures time to evaluate side-effects of @x@
-- and compute its normal form (by means of 'force').
--
-- Pure subexpression of an effectful computation @x@
-- may be evaluated only once and get cached.
-- To avoid surprising results it is usually preferable
-- to use 'nfAppIO' instead.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios,
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- A typical use case is 'nfIO' ('readFile' @"foo.txt"@).
-- However, if your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for @Criterion.@'Criterion.nfIO' and @Gauge.@'Gauge.nfIO'.
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = ioToBench force
{-# INLINE nfIO #-}

-- | 'whnfIO' @x@ measures time to evaluate side-effects of @x@
-- and compute its weak head normal form.
--
-- Pure subexpression of an effectful computation @x@
-- may be evaluated only once and get cached.
-- To avoid surprising results it is usually preferable
-- to use 'whnfAppIO' instead.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nfIO' instead.
--
-- Lazy I\/O is treacherous.
-- If your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for @Criterion.@'Criterion.whnfIO' and @Gauge.@'Gauge.whnfIO'.
whnfIO :: IO a -> Benchmarkable
whnfIO = ioToBench id
{-# INLINE whnfIO #-}

ioFuncToBench :: (b -> c) -> (a -> IO b) -> a -> Benchmarkable
ioFuncToBench frc f x = Benchmarkable $ do
    val <- f x
    _ <- evaluate (frc val)
    return ()
{-# OPAQUE ioFuncToBench #-}

-- | 'nfAppIO' @f@ @x@ measures time to evaluate side-effects of
-- an application of @f@ to @x@.
-- and compute its normal form (by means of 'force').
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentially @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios,
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- A typical use case is 'nfAppIO' 'readFile' @"foo.txt"@.
-- However, if your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for @Criterion.@'Criterion.nfAppIO' and @Gauge.@'Gauge.nfAppIO'.
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO = ioFuncToBench force
{-# INLINE nfAppIO #-}

-- | 'whnfAppIO' @f@ @x@ measures time to evaluate side-effects of
-- an application of @f@ to @x@.
-- and compute its weak head normal form.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentially @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nfAppIO' instead.
--
-- Lazy I\/O is treacherous.
-- If your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for @Criterion.@'Criterion.whnfAppIO' and @Gauge.@'Gauge.whnfAppIO'.
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO = ioFuncToBench id
{-# INLINE whnfAppIO #-}

-- | Run benchmarks in the given environment, usually reading large input data from file.
--
-- One might wonder why 'env' is needed,
-- when we can simply read all input data
-- before calling 'defaultMain'. The reason is that large data
-- dangling in the heap causes longer garbage collection
-- and slows down all benchmarks, even those which do not use it at all.
--
-- It is instrumental not only for proper 'IO' actions,
-- but also for a large statically-known data as well. Instead of a top-level
-- definition, which once evaluated will slow down garbage collection
-- during all subsequent benchmarks,
--
-- > largeData :: String
-- > largeData = replicate 1000000 'a'
-- >
-- > main :: IO ()
-- > main = defaultMain
-- >   [ bench "large" $ nf length largeData, ... ]
--
-- use
--
-- > import Control.DeepSeq (force)
-- > import Control.Exception (evaluate)
-- >
-- > main :: IO ()
-- > main = defaultMain
-- >   [ env (evaluate (force (replicate 1000000 'a'))) $ \largeData ->
-- >     bench "large" $ nf length largeData, ... ]
--
-- @Test.Tasty.Bench.@'env' is provided only for the sake of
-- compatibility with @Criterion.@'Criterion.env' and
-- @Gauge.@'Gauge.env', and involves 'unsafePerformIO'. Consider using
-- 'withResource' instead.
--
-- 'defaultMain' requires that the hierarchy of benchmarks and their names is
-- independent of underlying 'IO' actions. While executing 'IO' inside 'bench'
-- via 'nfIO' is fine, and reading test data from files via 'env' is also fine,
-- using 'env' to choose benchmarks or their names depending on 'IO' side effects
-- will throw a rather cryptic error message:
--
-- > Unhandled resource. Probably a bug in the runner you're using.
env :: NFData env => IO env -> (env -> Benchmark) -> Benchmark
env res = envWithCleanup res (const $ pure ())

-- | Similar to 'env', but includes an additional argument
-- to clean up created environment.
--
-- Provided only for the sake of compatibility with
-- @Criterion.@'Criterion.envWithCleanup' and
-- @Gauge.@'Gauge.envWithCleanup', and involves
-- 'unsafePerformIO'. Consider using 'withResource' instead.
envWithCleanup :: NFData env => IO env -> (env -> IO a) -> (env -> Benchmark) -> Benchmark
envWithCleanup res fin f = withResource
  (res >>= evaluate . force)
  (void . fin)
  (f . unsafePerformIO)

-- | A path to write results in CSV format, populated by @--csv@.
--
-- This is an option of 'csvReporter' and can be set only globally.
-- Modifying it via 'adjustOption' or 'localOption' does not have any effect.
-- One can however pass it to 'tryIngredients' 'benchIngredients'. For example,
-- here is how to set a default CSV location:
--
-- @
-- import Data.Maybe
-- import System.Exit
-- import Test.Tasty.Bench
-- import Test.Tasty.Ingredients
-- import Test.Tasty.Options
-- import Test.Tasty.Runners
--
-- main :: IO ()
-- main = do
--   let benchmarks = bgroup \"All\" ...
--   opts <- parseOptions benchIngredients benchmarks
--   let opts' = changeOption (Just . fromMaybe (CsvPath "foo.csv")) opts
--   case tryIngredients benchIngredients opts' benchmarks of
--     Nothing -> exitFailure
--     Just mb -> mb >>= \\b -> if b then exitSuccess else exitFailure
-- @
newtype CsvPath = CsvPath FilePath
  deriving (Typeable)

instance IsOption (Maybe CsvPath) where
  defaultValue = Nothing
  parseValue = Just . Just . CsvPath
  optionName = pure "csv"
  optionHelp = pure "File to write results in CSV format"

-- | Run benchmarks and save results in CSV format.
-- It activates when @--csv@ @FILE@ command line option is specified.
csvReporter :: Ingredient
csvReporter = TestReporter [Option (Proxy :: Proxy (Maybe CsvPath))] $
  \opts tree -> do
    CsvPath path <- lookupOption opts
    let names = testsNames opts tree
        namesMap = IM.fromDistinctAscList $ zip [0..] names
    pure $ \smap -> do
      case findNonUniqueElement names of
        Nothing -> pure ()
        Just name -> do -- 'die' is not available before base-4.8
          hPutStrLn stderr $ "CSV report cannot proceed, because name '" ++ name ++ "' corresponds to two or more benchmarks. Please disambiguate them."
          exitFailure
      let augmented = IM.intersectionWith (,) namesMap smap
      bracket
        (do
          h <- openFile path WriteMode
          hSetBuffering h LineBuffering
          hPutStrLn h $ "Name,Instructions"
          pure h
        )
        hClose
        $ \h -> csvOutput h augmented
      pure $ const $ isSuccessful smap

isSuccessful :: StatusMap -> IO Bool
isSuccessful = go . IM.elems
  where
    go [] = pure True
    go (tv : tvs) = do
      b <- atomically $ readTVar tv >>= \s -> case s of Done r -> pure (resultSuccessful r); _ -> retry
      if b then go tvs else pure False

findNonUniqueElement :: Ord a => [a] -> Maybe a
findNonUniqueElement = go S.empty
  where
    go _ [] = Nothing
    go acc (x : xs)
      | x `S.member` acc = Just x
      | otherwise = go (S.insert x acc) xs

csvOutput :: Handle -> IntMap (TestName, TVar Status) -> IO ()
csvOutput h = traverse_ $ \(name, tv) -> do
  r <- atomically $ readTVar tv >>= \s -> case s of Done r -> pure r; _ -> retry
  case safeRead (resultDescription r) of
    Nothing -> pure ()
    Just (WithLoHi est _ _) -> do
      msg <- formatMessage $ csvEncodeRow
          [ name
          , show (measInstructions est)
          , show (measBranchTotal est)
          , show (measBranchMisses est)
          , show (measCacheTotal est)
          , show (measCacheMisses est)
          , show (measMajorGC est)
          , show (measMinorGC est)
          ]
      hPutStrLn h msg

-- | A path to read baseline results in CSV format, populated by @--baseline@.
--
-- This is an option of 'csvReporter' and can be set only globally.
-- Modifying it via 'adjustOption' or 'localOption' does not have any effect.
-- One can however pass it to 'tryIngredients' 'benchIngredients'.
newtype BaselinePath = BaselinePath FilePath
  deriving (Typeable)

instance IsOption (Maybe BaselinePath) where
  defaultValue = Nothing
  parseValue = Just . Just . BaselinePath
  optionName = pure "baseline"
  optionHelp = pure "File with baseline results in CSV format to compare against"

-- | Run benchmarks and report results
-- in a manner similar to 'consoleTestReporter'.
--
-- If @--baseline@ @FILE@ command line option is specified,
-- compare results against an earlier run and mark
-- too slow / too fast benchmarks as failed in accordance to
-- bounds specified by @--fail-if-slower@ @PERCENT@ and @--fail-if-faster@ @PERCENT@.
consoleBenchReporter :: Ingredient
consoleBenchReporter = modifyConsoleReporter [Option (Proxy :: Proxy (Maybe BaselinePath))] $ \opts -> do
  baseline <- case lookupOption opts of
    Nothing -> pure M.empty
    Just (BaselinePath path) -> mkBaseline . csvDecodeTable <$> (readFile path >>= evaluate . force)

  pure $ \name mDepR r -> case safeRead (resultDescription r) of
    Nothing  -> r
    Just (WithLoHi est lowerBound upperBound) ->
      (if isAcceptable then id else forceFail)
      r { resultDescription = prettyEstimate est baseline' }
      where
        baseline' :: Maybe Estimate
        baseline' = M.lookup name baseline >>= \cells -> case cells of
          x1:x2:x3:x4:x5:x6:x7:_ -> pure Measurement
            <*> safeRead x1
            <*> safeRead x2
            <*> safeRead x3
            <*> safeRead x4
            <*> safeRead x5
            <*> safeRead x6
            <*> safeRead x7
          _ -> Nothing

        isAcceptable = isAcceptableVsBaseline && isAcceptableVsBcompare
        mSlowDown = compareVsBaseline baseline name est
        slowDown = fromMaybe 1 mSlowDown
        isAcceptableVsBaseline = slowDown >= lowerBound && slowDown <= upperBound
        (isAcceptableVsBcompare, _bcompareMsg) = case mDepR of
          Nothing -> (True, "")
          Just (WithLoHi depR depLowerBound depUpperBound) -> case safeRead (resultDescription depR) of
            Nothing -> (True, "")
            Just (WithLoHi depEst _ _) -> let ratio = estValue est / estValue depEst in
              ( ratio >= depLowerBound && ratio <= depUpperBound
              , printf ", %.2fx" ratio
              )
  where
    mkBaseline :: [[String]] -> M.Map String [String]
    mkBaseline table = M.fromList [ (name, row) | name:row <- table ]

estValue :: Estimate -> Double
estValue = word64ToDouble . measInstructions

compareVsBaseline :: M.Map String [String] -> TestName -> Estimate -> Maybe Double
compareVsBaseline baseline name m = case mOld of
  Nothing -> Nothing
  Just oldTime -> Just $ int64ToDouble time / int64ToDouble oldTime
  where
    time = word64ToInt64 $ measInstructions m

    mOld :: Maybe Int64
    mOld = do
      case M.lookup name baseline of
        Nothing       -> Nothing
        Just []       -> Nothing
        Just (cell:_) -> safeRead cell

{-
formatSlowDown :: Maybe Double -> String
formatSlowDown Nothing = ""
formatSlowDown (Just ratio) = case percents `compare` 0 of
  LT -> printf ", %2i%% less than baseline" (-percents)
  EQ -> ",       same as baseline"
  GT -> printf ", %2i%% more than baseline" percents
  where
    percents :: Int64
    percents = truncate ((ratio - 1) * 100)
-}

forceFail :: Result -> Result
forceFail r = r { resultOutcome = Failure TestFailed, resultShortDescription = "FAIL" }

data Unique a = None | Unique !a | NotUnique
  deriving (Functor)

appendUnique :: Unique a -> Unique a -> Unique a
appendUnique None a = a
appendUnique a None = a
appendUnique _ _ = NotUnique

instance Semigroup (Unique a) where
  (<>) = appendUnique

instance Monoid (Unique a) where
  mempty = None
  mappend = (<>)

modifyConsoleReporter
    :: [OptionDescription]
    -> (OptionSet -> IO (TestName -> Maybe (WithLoHi Result) -> Result -> Result))
    -> Ingredient
modifyConsoleReporter desc' iof = TestReporter (desc ++ desc') $ \opts tree ->
  let nameSeqs     = IM.fromDistinctAscList $ zip [0..] $ testNameSeqs opts tree
      namesAndDeps = IM.fromDistinctAscList $ zip [0..] $ map (second isSingle)
                   $ testNamesAndDeps nameSeqs opts tree
      modifySMap   = (iof opts >>=) . flip postprocessResult
                   . IM.intersectionWith (\(a, b) c -> (a, b, c)) namesAndDeps
  in (modifySMap >=>) <$> cb opts tree
  where
    (desc, cb) = case consoleTestReporter of
      TestReporter d c -> (d, c)
      _ -> error "modifyConsoleReporter: consoleTestReporter must be TestReporter"

    isSingle (Unique a) = Just a
    isSingle _ = Nothing

testNameSeqs :: OptionSet -> TestTree -> [Seq TestName]
testNameSeqs = foldTestTree trivialFold
  { foldSingle = const $ const . (:[]) . Seq.singleton
  , foldGroup  = const $ map . (<|)
  }

testNamesAndDeps :: IntMap (Seq TestName) -> OptionSet -> TestTree -> [(TestName, Unique (WithLoHi IM.Key))]
testNamesAndDeps im = foldTestTree trivialFold
  { foldSingle = const $ const . (: []) . (, mempty)
  , foldGroup  = const $ map . first . (++) . (++ ".")
  , foldAfter  = const foldDeps
  }
  where
    foldDeps :: DependencyType -> Expr -> [(a, Unique (WithLoHi IM.Key))] -> [(a, Unique (WithLoHi IM.Key))]
    foldDeps AllSucceed (And (StringLit xs) p)
      | bcomparePrefix `isPrefixOf` xs
      , Just (lo :: Double, hi :: Double) <- safeRead $ drop (length bcomparePrefix) xs
      = map $ second $ mappend $ (\x -> WithLoHi x lo hi) <$> findMatchingKeys im p
    foldDeps _ _ = id

findMatchingKeys :: IntMap (Seq TestName) -> Expr -> Unique IM.Key
findMatchingKeys im pattern =
  foldMap (\(k, v) -> if withFields v pat == Right True then Unique k else mempty) $ IM.assocs im
  where
    pat = eval pattern >>= asB


postprocessResult
    :: (TestName -> Maybe (WithLoHi Result) -> Result -> Result)
    -> IntMap (TestName, Maybe (WithLoHi IM.Key), TVar Status)
    -> IO StatusMap
postprocessResult f src = do
  paired <- forM src $ \(name, mDepId, tv) -> (name, mDepId, tv,) <$> newTVarIO NotStarted
  let doUpdate = atomically $ do
        (Any anyUpdated, All allDone) <-
          getApp $ flip foldMap paired $ \(name, mDepId, newTV, oldTV) -> Ap $ do
            old <- readTVar oldTV
            case old of
              Done{} -> pure (Any False, All True)
              _ -> do
                new <- readTVar newTV
                case new of
                  Done res -> do

                    depRes <- case mDepId of
                      Nothing -> pure Nothing
                      Just (WithLoHi depId lo hi) -> case IM.lookup depId src of
                        Nothing -> pure Nothing
                        Just (_, _, depTV) -> do
                          depStatus <- readTVar depTV
                          case depStatus of
                            Done dep -> pure $ Just (WithLoHi dep lo hi)
                            _ -> pure Nothing

                    writeTVar oldTV (Done (f name depRes res))
                    pure (Any True, All True)
                  -- ignoring Progress nodes, we do not report any
                  -- it would be helpful to have instance Eq Progress
                  _ -> pure (Any False, All False)
        if anyUpdated || allDone then pure allDone else retry
      adNauseam = doUpdate >>= (`unless` adNauseam)
  _ <- forkIO adNauseam
  pure $ fmap (\(_, _, _, a) -> a) paired

int64ToDouble :: Int64 -> Double
int64ToDouble = fromIntegral

word64ToInt64 :: Word64 -> Int64
word64ToInt64 = fromIntegral

word64ToDouble :: Word64 -> Double
word64ToDouble = fromIntegral

#if !MIN_VERSION_base(4,10,0)
int64ToWord64 :: Int64 -> Word64
int64ToWord64 = fromIntegral
#endif

-- | Map leaf benchmarks ('bench', not 'bgroup') with a provided function,
-- which has an access to leaf's reversed path.
--
-- This helper is useful for bulk application of 'bcompare'.
-- See also 'locateBenchmark'.
--
-- Real world example: https://hackage.haskell.org/package/text-builder-linear-0.1/src/bench/Main.hs
mapLeafBenchmarks :: ([String] -> Benchmark -> Benchmark) -> Benchmark -> Benchmark
mapLeafBenchmarks processLeaf = go mempty
  where
    go :: [String] -> Benchmark -> Benchmark
    go path x = case x of
      SingleTest name t    -> processLeaf (name : path) (SingleTest name t)
      TestGroup name tts   -> TestGroup name (map (go (name : path))  tts)
      PlusTestOptions g tt -> PlusTestOptions g (go path tt)
      WithResource res f   -> WithResource res (go path . f)
      AskOptions f         -> AskOptions (go path . f)
      After dep expr tt    -> After dep expr (go path tt)

-- | Construct an AWK expression to locate an individual element or elements in 'Benchmark'
-- by the suffix of the path. Names are listed in reverse order:
-- from 'bench'\'s own name to a name of the outermost 'bgroup'.
--
-- This function is meant to be used in conjunction with 'bcompare', e. g.,
-- 'bcompare' ('Test.Tasty.Patterns.Printer.printAwkExpr' ('locateBenchmark' @path@)).
-- See also 'mapLeafBenchmarks'.
--
-- This function requires @tasty-1.0@, so if you use 'locateBenchmark'
-- it is prudent to add @tasty >= 1.0@ to @build-depends@
-- section of your cabal file.
--
-- Real world example: https://hackage.haskell.org/package/text-builder-linear-0.1/src/bench/Main.hs
locateBenchmark :: [String] -> Expr
locateBenchmark [] = IntLit 1
locateBenchmark path
  = foldl1' And
  $ zipWith (\i name -> Patterns.EQ (Field (Sub NF (IntLit i))) (StringLit name)) [0..] path

-------------------------------------------------------------------------------
-- Csv: Encoding
-------------------------------------------------------------------------------

csvEncodeRow :: [String] -> String
csvEncodeRow []     = ""
csvEncodeRow [x]    = csvEncodeField x
csvEncodeRow (x:xs) = csvEncodeField x ++ "," ++ csvEncodeRow xs

csvEncodeField :: String -> String
csvEncodeField xs
  | any (`elem` xs) ",\"\n\r"
  = '"' : go xs -- opening quote

  | otherwise = xs
  where
    go []         = '"' : []
    go ('"' : ys) = '"' : '"' : go ys
    go (y : ys)   = y : go ys

-------------------------------------------------------------------------------
-- Csv: Decoding
-------------------------------------------------------------------------------

-- | Decode CSV trying to recover as much as possible.
csvDecodeTable :: String -> [[String]]
csvDecodeTable []                 = []
csvDecodeTable ('\r' : '\n' : cs) = csvDecodeTable cs
csvDecodeTable ('\r'        : cs) = csvDecodeTable cs
csvDecodeTable ('\n'        : cs) = csvDecodeTable cs
csvDecodeTable (','         : cs) = csvDecodeField ("" :) cs
csvDecodeTable ('"'         : cs) = csvDecodeEscapedField id id cs
csvDecodeTable (c           : cs) = csvDecodeUnescapedField (c :) id cs

csvDecodeField :: ([String] -> [String]) -> String -> [[String]]
csvDecodeField accR ('\r' : '\n' : cs) = accR [""] : csvDecodeTable cs
csvDecodeField accR ('\r'        : cs) = accR [""] : csvDecodeTable cs
csvDecodeField accR ('\n'        : cs) = accR [""] : csvDecodeTable cs
csvDecodeField accR ('"'         : cs) = csvDecodeEscapedField id accR cs
csvDecodeField accR (','         : cs) = csvDecodeField (accR . ("" :)) cs
csvDecodeField accR (c           : cs) = csvDecodeUnescapedField (c :) accR cs
csvDecodeField accR []                 = [accR []]

csvDecodeEscapedField :: (String -> String) -> ([String] -> [String]) -> String -> [[String]]
csvDecodeEscapedField accF accR ('"' : '"' : cs) = csvDecodeEscapedField (accF . ('"' :)) accR cs
csvDecodeEscapedField accF accR ('"' : cs)       = csvDecodeAfterEscapedField (accR . (accF "" :)) cs
csvDecodeEscapedField accF accR (c   : cs)       = csvDecodeEscapedField (accF . (c :))   accR cs
csvDecodeEscapedField accF accR []               = [accR [accF ""]]

-- expected: EOF, EOL or ,
csvDecodeAfterEscapedField :: ([String] -> [String]) -> String -> [[String]]
csvDecodeAfterEscapedField accR [] = [accR []]
csvDecodeAfterEscapedField accR ('\r' : '\n' : cs) = accR [] : csvDecodeTable cs
csvDecodeAfterEscapedField accR ('\r'        : cs) = accR [] : csvDecodeTable cs
csvDecodeAfterEscapedField accR ('\n'        : cs) = accR [] : csvDecodeTable cs
csvDecodeAfterEscapedField accR (','         : cs) = csvDecodeField accR cs
csvDecodeAfterEscapedField accR (_           : cs) = csvDecodeAfterEscapedField accR cs

csvDecodeUnescapedField :: (String -> String) -> ([String] -> [String]) -> String -> [[String]]
csvDecodeUnescapedField accF accR (','         : cs) = csvDecodeField (accR . (accF "" :)) cs
csvDecodeUnescapedField accF accR ('\r' : '\n' : cs) = accR [accF ""] : csvDecodeTable cs
csvDecodeUnescapedField accF accR ('\r'        : cs) = accR [accF ""] : csvDecodeTable cs
csvDecodeUnescapedField accF accR ('\n'        : cs) = accR [accF ""] : csvDecodeTable cs
csvDecodeUnescapedField accF accR (c           : cs) = csvDecodeUnescapedField (accF . (c :)) accR cs
csvDecodeUnescapedField accF accR []                 = [accR [accF ""]]
