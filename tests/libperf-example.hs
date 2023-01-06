{-# LANGUAGE CApiFFI #-}
module Main (main) where

import Text.Printf       (printf)

import LibPerf

main :: IO ()
main = checkRoot $ withPerf HwInstructions $ \h -> do
    let report :: IO ()
        report = do
            v <- perfRead h
            printf "instruction count: %u\n" v

    putStrLn "After opening"
    report

    perfReset h
    perfEnable h
    putStrLn "Measuring cpu cycles and instruction count for this putStrLn"
    perfDisable h

    report

    perfReset h
    perfEnable h
    putStrLn "Measuring cpu cycles and instruction count for another putStrLn"
    perfDisable h

    report

    perfReset h
    perfEnable h
    putStrLn "Measuring cpu cycles and instruction count for third putStrLn"
    perfDisable h

    report

checkRoot :: IO () -> IO ()
checkRoot action = do
    hasCapPerfMon <- perfHasCapability
    if hasCapPerfMon
    then action
    else putStrLn "perf needs CAP_PERFMON; run me as root"
