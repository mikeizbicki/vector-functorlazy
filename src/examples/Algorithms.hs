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
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.AmericanFlag as AmericanFlag

import qualified Data.Vector.FunctorLazy.LazyBox as FLV_LB
import qualified Data.Vector.FunctorLazy.DualVector as FLV_DV
import qualified Data.Vector.FunctorLazy.DualArray as FLV_DA
-- import qualified Data.Vector.FunctorLazy.DualVector as FLV_D

myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 5
    }

main = do
    --let xs = evalRand (mkStdGen 0) $ replicateM 1000 $ getRandomR (1,100)
    xs <- evalRandIO $ replicateM 10000 $ getRandomR (1,100)
    vum    :: VUM.IOVector Int <- VG.thaw (VG.fromList xs :: VU.Vector Int)
    vm     :: VM.IOVector Int <- VG.thaw $ (VG.fromList xs :: V.Vector Int)
    flv_lb :: FLV_LB.MVector RealWorld Int <- VG.basicUnsafeThaw (VG.fromList xs :: FLV_LB.Vector Int)
    flv_dv :: FLV_DV.MVector RealWorld Int <- VG.basicUnsafeThaw (VG.fromList xs :: FLV_DV.Vector Int)
    flv_da :: FLV_DA.MVector RealWorld Int <- VG.basicUnsafeThaw (VG.fromList xs :: FLV_DA.Vector Int)
--     flv_d1 :: FLV_D.MVector RealWorld Int <- VG.basicUnsafeThaw (fmap (+1) $ VG.fromList xs :: FLV_D.Vector Int)

    defaultMainWith myConfig (return ())
        [ bench "heap-10000-vm" (Insertion.sort vm :: IO ())
        , bench "heap-10000-flv_dv" (Insertion.sort flv_dv :: IO ())
        , bench "heap-10000-flv_da" (Insertion.sort flv_da :: IO ())
--         [ bench "heap-10000-vum" (Heap.sort vum :: IO ())
--         , bench "heap-10000-vm" (Heap.sort vm :: IO ())
--         , bench "heap-10000-flv_lb" (Heap.sort flv_lb :: IO ())
--         , bench "heap-10000-flv_d" (Heap.sort flv_d :: IO ())
--         , bench "heap-10000-flv_d1" (Heap.sort flv_d1 :: IO ())
--         , bench "insertion-10000-vum" (Insertion.sort vum :: IO ())
--         , bench "insertion-10000-vm" (Insertion.sort vm :: IO ())
--         , bench "insertion-10000-flv_lb" (Insertion.sort flv_lb :: IO ())
--         , bench "insertion-10000-flv_d" (Insertion.sort flv_d :: IO ())
--         , bench "insertion-10000-flv_d1" (Insertion.sort flv_d1 :: IO ())
--
--         , bench "insertion-10000-v" (Insertion.sort v :: IO ())
--         , bench "insertion-10000-v'" (Insertion.sort v' :: IO ())
--         , bench "insertion-10000-FLV_D" (Insertion.sort flv_d :: IO ())
--         , bench "merge-10000-v" (Merge.sort v :: IO ())
--         , bench "merge-10000-v'" (Merge.sort v' :: IO ())
--         , bench "merge-10000-flv_d" (Merge.sort flv_d :: IO ())
--         , bench "radix-10000-v" (Radix.sort v :: IO ())
--         , bench "radix-10000-v'" (Radix.sort v' :: IO ())
--         , bench "radix-10000-flv_d" (Radix.sort flv_d :: IO ())
        ]
