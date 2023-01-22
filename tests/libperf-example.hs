{-# LANGUAGE CApiFFI           #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import Text.Printf (printf)

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import LibPerf

main :: IO ()
main = checkRoot $ do
    singleCounter
    group

singleCounter :: IO ()
singleCounter = withPerf HwInstructions $ \h -> do
    let report :: IO ()
        report = do
            v <- perfRead h
            printf "instruction count: %u\n" v

    putStrLn "After opening"
    report

    perfReset h
    perfEnable h
    putStrLn "Measuring instruction count for this putStrLn"
    perfDisable h

    report

    perfReset h
    perfEnable h
    putStrLn "Measuring instruction count for another putStrLn"
    perfDisable h

    report

    perfReset h
    perfEnable h
    putStrLn "Measuring instruction count for third putStrLn"
    perfDisable h

    report

data R a = R
    { rInstructions       :: a
    , rBranchInstructions :: a
    , rBranchMisses       :: a
    , rCacheReferences    :: a
    , rCacheMisses        :: a
    }
  deriving (Functor, F.Foldable, T.Traversable)

group :: IO ()
group = withPerfGroup (R HwInstructions HwBranchInstructions HwBranchMisses HwCacheReferences HwCacheMisses) $ \h -> do
    let report :: IO ()
        report = do
            R {..} <- perfGroupRead h
            printf "instruction count:   %u\n" rInstructions
            printf "branch instructions: %u\n" rBranchInstructions
            printf "branch misses:       %u\n" rBranchMisses
            printf "cache references:    %u\n" rCacheReferences
            printf "cache misses:        %u\n" rCacheMisses

    putStrLn "After opening"
    report

    perfGroupReset h
    perfGroupEnable h
    putStrLn "Measuring many things for this putStrLn"
    perfGroupDisable h

    report

checkRoot :: IO () -> IO ()
checkRoot action = do
    hasCapPerfMon <- perfHasCapability
    if hasCapPerfMon
    then action
    else putStrLn "perf needs CAP_PERFMON; run me as root"
