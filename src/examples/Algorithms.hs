{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Config
import Criterion.Main
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST.Safe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import qualified Data.Vector.Algorithms.Insertion as Insertion
import qualified Data.Vector.Algorithms.Radix as Radix
import qualified Data.Vector.Algorithms.Heap as Heap
import qualified Data.Vector.Algorithms.Merge as Merge

import qualified Data.Vector.SuperLazy as SLV
import qualified Data.Vector.SuperLazy.DualVector as SLV_D

myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 5
    }

main = do
    --let xs = evalRand (mkStdGen 0) $ replicateM 1000 $ getRandomR (1,100)
    xs <- evalRandIO $ replicateM 100000 $ getRandomR (1,100)
    vum   :: VUM.IOVector Int <- VG.thaw (VG.fromList xs :: VU.Vector Int)
    vm    :: VM.IOVector Int <- VG.thaw $ (VG.fromList xs :: V.Vector Int)
    slv   :: SLV.MVector RealWorld Int <- VG.basicUnsafeThaw (VG.fromList xs :: SLV.Vector Int)
    slv_d :: SLV_D.MVector RealWorld Int <- VG.basicUnsafeThaw (VG.fromList xs :: SLV_D.Vector Int)
    slv_d1 :: SLV_D.MVector RealWorld Int <- VG.basicUnsafeThaw (fmap (+1) $ VG.fromList xs :: SLV_D.Vector Int)

    defaultMainWith myConfig (return ())
        [ bench "heap-10000-vum" (Heap.sort vum :: IO ())
        , bench "heap-10000-vm" (Heap.sort vm :: IO ())
        , bench "heap-10000-slv" (Heap.sort slv :: IO ())
        , bench "heap-10000-slv_d" (Heap.sort slv_d :: IO ())
        , bench "heap-10000-slv_d1" (Heap.sort slv_d1 :: IO ())
--         , bench "insertion-10000-v" (Insertion.sort v :: IO ())
--         , bench "insertion-10000-v'" (Insertion.sort v' :: IO ())
--         , bench "insertion-10000-SLV_D" (Insertion.sort slv_d :: IO ())
--         , bench "merge-10000-v" (Merge.sort v :: IO ())
--         , bench "merge-10000-v'" (Merge.sort v' :: IO ())
--         , bench "merge-10000-slv_d" (Merge.sort slv_d :: IO ())
--         , bench "radix-10000-v" (Radix.sort v :: IO ())
--         , bench "radix-10000-v'" (Radix.sort v' :: IO ())
--         , bench "radix-10000-slv_d" (Radix.sort slv_d :: IO ())
        ]
