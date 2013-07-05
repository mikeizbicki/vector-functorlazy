{-# LANGUAGE ScopedTypeVariables,RankNTypes #-}

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

import qualified Data.Vector.FunctorLazy as VFL

import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import System.IO.Unsafe
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main = defaultMain
    [ testGroup "no fmap"
        [ testProperty "Heap.sort" (isCorrect Heap.sort)
        , testProperty "Insertion.sort" (isCorrect Insertion.sort)
        , testProperty "Intro.sort" (isCorrect Intro.sort)
        , testProperty "Radix.sort" (isCorrect Radix.sort)
        ]
    , testGroup "with fmap"
        [ testProperty "Heap.sort" (isCorrectFunctor Heap.sort)
        , testProperty "Insertion.sort" (isCorrectFunctor Insertion.sort)
        , testProperty "Intro.sort" (isCorrectFunctor Intro.sort)
        , testProperty "Radix.sort" (isCorrectFunctor Radix.sort)
        ]
    ]

-- | tests the correctness of Data.Vector.FunctorLazy by checking whether operations on boxed vectors and functor lazy vectors get the same values
isCorrect :: (forall v. VGM.MVector v Int => v RealWorld Int -> IO ())  -> [Int] -> Bool
isCorrect sort xs = unsafePerformIO $ do
    vm  :: VM.MVector  RealWorld Int <- VG.thaw (VG.fromList xs :: V.Vector Int)
    vfl :: VFL.MVector RealWorld Int <- VG.thaw (VG.fromList xs :: VFL.Vector Int)
    sort vm
    sort vfl
    ys_vm  :: V.Vector Int   <- VG.freeze vm
    ys_vfl :: VFL.Vector Int <- VG.freeze vfl 
    return $ VG.toList ys_vm == VG.toList ys_vfl

-- | same as isCorrect, but applies the fmap function before sorting
isCorrectFunctor :: (forall v. VGM.MVector v Int => v RealWorld Int -> IO ())  -> [Int] -> Bool
isCorrectFunctor sort xs = unsafePerformIO $ do
    vm  :: VM.MVector  RealWorld Int <- VG.thaw (dofmap $ VG.fromList xs :: V.Vector Int)
    vfl :: VFL.MVector RealWorld Int <- VG.thaw (dofmap $ VG.fromList xs :: VFL.Vector Int)
    sort vm
    sort vfl
    ys_vm  :: V.Vector Int   <- VG.freeze vm
    ys_vfl :: VFL.Vector Int <- VG.freeze vfl 
    return $ VG.toList ys_vm == VG.toList ys_vfl
    where
        funcL = [(+1),(*2),(^2),(\x -> x-1),(`mod` 57)] :: [Int -> Int]
        dofmap f = go funcL f
        go [] f = f
        go (x:xs) f = go xs $ fmap x f
