{-# LANGUAGE CApiFFI #-}
module LibPerf (
    -- * Performance counters
    PerfCounter (..),
    -- * Performance counter handle
    PerfHandle,
    withPerf,
    perfRead,
    perfEnable,
    perfDisable,
    perfReset,
    -- ** Low-level primitives
    perfOpen,
    perfClose,
    -- * Capability check
    perfHasCapability,
) where

import Control.Exception (bracket)
import Data.Word         (Word64)
import Foreign.C.Error   (throwErrnoIf, throwErrnoIf_)
import Foreign.C.Types   (CInt (..))

-- | Available performance counters.
--
-- (Only subset is exposed, ask if you need more).
data PerfCounter
    = HwCpuCycles           -- ^ Total cycles.  Be wary of what happens during CPU frequency scaling.
    | HwInstructions        -- ^ Retired instructions.  Be careful, these can be affected by various issues, most notably hardware interrupt counts.
    | HwCacheReferences     -- ^ Cache accesses.
    | HwCacheMisses         -- ^ Cache misses.
    | HwBranchInstructions  -- ^ Retired branch instructions.
    | HwBranchMisses        -- ^ Mispredicted branch instructions.
    | HwRefCpuCycles        -- ^ Total cycles; not affected by CPU frequency scaling.
  deriving (Eq, Show)

newtype PerfHandle = PerfHandle CInt

-- | Bracket of 'perfOpen' and 'perfClose'.
--
-- Counters are opened disabled. Use 'perfEnable' to enable them.
--
withPerf :: PerfCounter -> (PerfHandle -> IO r) -> IO r
withPerf c = bracket (perfOpen c) perfClose

-- | Open performance counter handle.
--
-- Counters are opened disabled. Use 'perfEnable' to enable them.
--
perfOpen :: PerfCounter -> IO PerfHandle
perfOpen c = fmap PerfHandle (throwErrnoIf (< 0) "perf_event_open" (cLibPerfOpen c'))
  where
    c' :: CInt
    c' = case c of
        HwCpuCycles          -> cLibPerf_HW_CPU_CYCLES
        HwInstructions       -> cLibPerf_HW_INSTRUCTIONS
        HwCacheReferences    -> cLibPerf_HW_CACHE_REFERENCES
        HwCacheMisses        -> cLibPerf_HW_CACHE_MISSES
        HwBranchInstructions -> cLibPerf_HW_BRANCH_INSTRUCTIONS
        HwBranchMisses       -> cLibPerf_HW_BRANCH_MISSES
        HwRefCpuCycles       -> cLibPerf_HW_REF_CPU_CYCLES

-- | Close performance counter.
perfClose :: PerfHandle -> IO ()
perfClose (PerfHandle h) = throwErrnoIf_ (< 0) "close" (cLibPerfClose h)

-- | Enable performance counter.
perfEnable :: PerfHandle -> IO ()
perfEnable (PerfHandle h) = throwErrnoIf_ (< 0) "ioctl" (cLibPerfEnable h)

-- | Disable performance counter.
perfDisable :: PerfHandle -> IO ()
perfDisable (PerfHandle h) = throwErrnoIf_ (< 0) "ioctl" (cLibPerfDisable h)

-- | Reset performance counter.
perfReset :: PerfHandle -> IO ()
perfReset (PerfHandle h) = throwErrnoIf_ (< 0) "ioctl" (cLibPerfReset h)

-- | Read the value of the performance counter.
perfRead :: PerfHandle -> IO Word64
perfRead (PerfHandle h) = cLibPerfRead h

-- | Test whether running process has required @CAP_PERFMON@ (or @CAP_SYS_ADMIN@ on older kernels) capability.
perfHasCapability :: IO Bool
perfHasCapability = do
    i <- cLibPerfHasCapPerfMon
    return (i /= 0)

-------------------------------------------------------------------------------
-- FFI imports
-------------------------------------------------------------------------------

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_HW_CPU_CYCLES"
    cLibPerf_HW_CPU_CYCLES :: CInt

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_HW_INSTRUCTIONS"
    cLibPerf_HW_INSTRUCTIONS :: CInt

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_HW_CACHE_REFERENCES"
    cLibPerf_HW_CACHE_REFERENCES :: CInt

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_HW_CACHE_MISSES"
    cLibPerf_HW_CACHE_MISSES :: CInt

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_HW_BRANCH_INSTRUCTIONS"
    cLibPerf_HW_BRANCH_INSTRUCTIONS :: CInt

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_HW_BRANCH_MISSES"
    cLibPerf_HW_BRANCH_MISSES :: CInt

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_HW_REF_CPU_CYCLES"
    cLibPerf_HW_REF_CPU_CYCLES :: CInt

foreign import capi safe "HsLibPerf.h HsLibPerfOpen"
    cLibPerfOpen :: CInt -> IO CInt

foreign import capi safe "HsLibPerf.h HsLibPerfRead"
    cLibPerfRead :: CInt -> IO Word64

foreign import capi safe "HsLibPerf.h HsLibPerfEnable"
    cLibPerfEnable :: CInt -> IO CInt

foreign import capi safe "HsLibPerf.h HsLibPerfReset"
    cLibPerfReset :: CInt -> IO CInt

foreign import capi safe "HsLibPerf.h HsLibPerfDisable"
    cLibPerfDisable :: CInt -> IO CInt

foreign import capi safe "HsLibPerf.h close"
    cLibPerfClose :: CInt -> IO CInt

foreign import capi safe "HsLibPerf.h HsLibPerfHasCapPerfMon"
    cLibPerfHasCapPerfMon :: IO CInt
