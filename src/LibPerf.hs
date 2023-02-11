{-# LANGUAGE CApiFFI    #-}
{-# LANGUAGE RankNTypes #-}
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
    -- * Group interface
    PerfGroupHandle,
    withPerfGroup,
    perfGroupRead,
    perfGroupEnable,
    perfGroupDisable,
    perfGroupReset,
    -- ** Low-level primitives
    perfGroupOpen,
    perfGroupClose,
    -- * Capability check
    perfHasCapability,
) where

import Control.Applicative   (Applicative (pure, (<*>)))
import Control.Exception     (bracket)
import Control.Monad         (unless)
import Data.Foldable         (traverse_)
import Data.IORef            (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Traversable      (Traversable (traverse), mapAccumL)
import Data.Word             (Word64)
import Foreign.C.Error       (throwErrnoIf, throwErrnoIf_)
import Foreign.C.Types       (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr           (Ptr)
import Prelude
       (Bool, Eq, Functor (fmap), IO, Int, Maybe (..), Monad (..), Show, fail,
       fromIntegral, snd, ($), (*), (+), (/=), (<), (==), (++), show)

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
    | SwPageFaults          -- ^ This reports the number of page faults.
    | SwDummy               -- ^ This is a placeholder event that counts nothing.
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
perfOpen c = fmap PerfHandle (throwErrnoIf (< 0) "perf_event_open" (cLibPerfOpen (perfCounterToCInt c) (-1) 0))

perfCounterToCInt :: PerfCounter -> CInt
perfCounterToCInt c = case c of
        HwCpuCycles          -> cLibPerf_HW_CPU_CYCLES
        HwInstructions       -> cLibPerf_HW_INSTRUCTIONS
        HwCacheReferences    -> cLibPerf_HW_CACHE_REFERENCES
        HwCacheMisses        -> cLibPerf_HW_CACHE_MISSES
        HwBranchInstructions -> cLibPerf_HW_BRANCH_INSTRUCTIONS
        HwBranchMisses       -> cLibPerf_HW_BRANCH_MISSES
        HwRefCpuCycles       -> cLibPerf_HW_REF_CPU_CYCLES
        SwPageFaults         -> cLibPerf_SW_PAGE_FAULTS
        SwDummy              -> cLibPerf_SW_DUMMY

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
-- Group
-------------------------------------------------------------------------------

data PerfGroupHandle f = PerfGroupHandle !CInt !Int (f CInt)

-- | (Morally a) bracket of 'perfGroupOpen' and 'perfGroupClose'.
--
withPerfGroup :: Traversable t => t PerfCounter -> (PerfGroupHandle t -> IO r) -> IO r
withPerfGroup cs0 kont = do
    gfdRef <- newIORef Nothing
    cntRef <- newIORef 0

    let acquire :: PerfCounter -> IO CInt
        acquire c = do
            mgfd <- readIORef gfdRef
            case mgfd of
                Nothing -> do
                    fd <- throwErrnoIf (< 0) "perf_event_open" (cLibPerfOpen (perfCounterToCInt c) (-1) 1)
                    writeIORef gfdRef (Just fd)
                    modifyIORef' cntRef (1 +)
                    return fd
                Just gfd -> do
                    fd <- throwErrnoIf (< 0) "perf_event_open" (cLibPerfOpen (perfCounterToCInt c) gfd 0)
                    modifyIORef' cntRef (1 +)
                    return fd

        release :: CInt -> IO ()
        release fd = throwErrnoIf_ (< 0) "close" (cLibPerfClose fd)

    let go :: PerfCounter -> Managed CInt
        go c = Managed (bracket (acquire c) release)

    with (traverse go cs0) $ \fds -> do
        mgfd <- readIORef gfdRef
        case mgfd of
            Nothing -> fail "Empty traversable in withPerfGroup"
            Just gfd -> do
                len <- readIORef cntRef
                kont (PerfGroupHandle gfd len fds)

-- | Open performance counter group.
perfGroupOpen :: Traversable t => t PerfCounter -> IO (PerfGroupHandle t)
perfGroupOpen cs = do
    gfdRef <- newIORef Nothing
    cntRef <- newIORef 0

    let acquire :: PerfCounter -> IO CInt
        acquire c = do
            mgfd <- readIORef gfdRef
            case mgfd of
                Nothing -> do
                    fd <- throwErrnoIf (< 0) "perf_event_open" (cLibPerfOpen (perfCounterToCInt c) (-1) 1)
                    writeIORef gfdRef (Just fd)
                    modifyIORef' cntRef (1 +)
                    return fd
                Just gfd -> do
                    fd <- throwErrnoIf (< 0) "perf_event_open" (cLibPerfOpen (perfCounterToCInt c) gfd 0)
                    modifyIORef' cntRef (1 +)
                    return fd

    fds <- traverse acquire cs
    mgfd <- readIORef gfdRef
    case mgfd of
        Nothing -> fail "Empty traversable in perfGroupOpen"
        Just gfd -> do
            len <- readIORef cntRef
            return (PerfGroupHandle gfd len fds)

-- | Close performance counter group.
perfGroupClose :: Traversable t => PerfGroupHandle t -> IO ()
perfGroupClose (PerfGroupHandle _ _ fds) = traverse_ cLibPerfClose fds

-- | Read the values of the performance counter group.
perfGroupRead :: Traversable t => PerfGroupHandle t -> IO (t Word64)
perfGroupRead (PerfGroupHandle h len fds) =
    allocaBytes (8 * len1) $ \ptr -> do
        r <- throwErrnoIf (< 0) "read" $ cLibPerfRawRead h (fromIntegral len1) ptr
        unless (fromIntegral r == 8 * len1) $ fail $ "read: didn't read enough: " ++ show r
        res <- peekArray (1 + len) ptr
        case res of
            []   -> fail "perfGroupRead: panic peekArray returned empty list"
            n:xs -> do
                unless (fromIntegral n == len) $ fail "read: less than expected counters"
                return $ snd (mapAccumL fill xs fds)
  where
    len1 :: Int
    len1 = len + 1

    fill :: [Word64] -> a -> ([Word64], Word64)
    fill []     _ = ([], 0)
    fill (w:ws) _ = (ws, w)

-- | Enable performance counter group.
perfGroupEnable :: PerfGroupHandle t -> IO ()
perfGroupEnable (PerfGroupHandle h _ _) = throwErrnoIf_ (< 0) "ioctl" (cLibPerfEnable h)

-- | Disable performance counter group.
perfGroupDisable :: PerfGroupHandle t -> IO ()
perfGroupDisable (PerfGroupHandle h _ _) = throwErrnoIf_ (< 0) "ioctl" (cLibPerfDisable h)

-- | Reset performance counter group.
perfGroupReset :: PerfGroupHandle t -> IO ()
perfGroupReset (PerfGroupHandle h _ _) = throwErrnoIf_ (< 0) "ioctl" (cLibPerfReset h)

-------------------------------------------------------------------------------
-- Managed
-------------------------------------------------------------------------------

-- from https://hackage.haskell.org/package/managed
-- (c) Gabriella Gonzalez under BSD-3-Clause
newtype Managed a = Managed { (>>-) :: forall r . (a -> IO r) -> IO r }

instance Functor Managed where
    fmap f mx = Managed $ \k ->
        mx >>- \x ->
        k (f x)

instance Applicative Managed where
    pure r    = Managed $ \k ->
        k r

    mf <*> mx = Managed $ \k->
        mf >>- \f ->
        mx >>- \x ->
        k (f x)

instance Monad Managed where
    return = pure

    ma >>= f = Managed $ \k->
        ma  >>- \a ->
        f a >>- \b ->
        k b

with :: Managed a -> (a -> IO r) -> IO r
with m = (>>-) m

-------------------------------------------------------------------------------
-- FFI imports: values
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

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_SW_PAGE_FAULTS"
    cLibPerf_SW_PAGE_FAULTS :: CInt

foreign import capi "HsLibPerf.h value HS_PERF_COUNT_SW_DUMMY"
    cLibPerf_SW_DUMMY :: CInt

-------------------------------------------------------------------------------
-- FFI imports: procedures
-------------------------------------------------------------------------------

foreign import capi safe "HsLibPerf.h HsLibPerfOpen"
    cLibPerfOpen :: CInt -> CInt -> CInt -> IO CInt

foreign import capi safe "HsLibPerf.h HsLibPerfRead"
    cLibPerfRead :: CInt -> IO Word64

foreign import capi safe "HsLibPerf.h HsLibPerfRawRead"
    cLibPerfRawRead :: CInt -> CInt -> Ptr Word64 -> IO CInt

foreign import capi safe "HsLibPerf.h HsLibPerfReset"
    cLibPerfReset :: CInt -> IO CInt

-- Enable and disable are unsafe to make RTS do less in between.
foreign import capi safe "HsLibPerf.h HsLibPerfEnable"
    cLibPerfEnable :: CInt -> IO CInt

foreign import capi safe "HsLibPerf.h HsLibPerfDisable"
    cLibPerfDisable :: CInt -> IO CInt

foreign import capi safe "HsLibPerf.h close"
    cLibPerfClose :: CInt -> IO CInt

foreign import capi safe "HsLibPerf.h HsLibPerfHasCapPerfMon"
    cLibPerfHasCapPerfMon :: IO CInt
