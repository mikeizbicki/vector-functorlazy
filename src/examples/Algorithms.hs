{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Config
import Criterion.Main
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST.Safe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import qualified Data.Vector.Algorithms.Insertion as Insertion
import qualified Data.Vector.Algorithms.Radix as Radix
import qualified Data.Vector.Algorithms.Heap as Heap
import qualified Data.Vector.Algorithms.Merge as Merge

import qualified Data.Vector.SuperLazy as SLV

myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 5
    }

--runTest :: VGM.MVector v e => v RealWorld e -> IO ()
runTest sort vec = sort vec

main = do
    --let xs = evalRand (mkStdGen 0) $ replicateM 1000 $ getRandomR (1,100)
    xs <- evalRandIO $ replicateM 10000 $ getRandomR (1,100)
    v  :: VM.IOVector Int <- VG.thaw $ (VG.fromList xs :: V.Vector Int)
    v' :: SLV.MVector RealWorld Int <- VG.basicUnsafeThaw (VG.fromList xs :: SLV.Vector Int)
    defaultMainWith myConfig (return ())
        --[]
        [ bench "heap-10000-v" (Heap.sort v :: IO ())
        , bench "heap-10000-v'" (Heap.sort v' :: IO ())
        , bench "insertion-10000-v" (Insertion.sort v :: IO ())
        , bench "insertion-10000-v'" (Insertion.sort v' :: IO ())
        , bench "merge-10000-v" (Merge.sort v :: IO ())
        , bench "merge-10000-v'" (Merge.sort v' :: IO ())
        , bench "radix-10000-v" (Radix.sort v :: IO ())
        , bench "radix-10000-v'" (Radix.sort v' :: IO ())
        ]
