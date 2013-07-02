{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables,TypeFamilies,FlexibleInstances,MultiParamTypeClasses #-}

module Data.Vector.SuperLazy.DualVector
    where

import Data.Monoid hiding (Any)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Vector.Unboxed.Deriving

import Control.Monad.ST
import Control.Monad.Primitive
import Unsafe.Coerce
import System.IO.Unsafe
import GHC.Prim

import Data.Vector.SuperLazy.Common

-- | Has the same semantics as Data.Vector, but offers an alternative time/space tradeoff.  In particular, super lazy Vectors make all calls to fmap happen in constant time; in traditional lazy vectors, fmap must be applied to each element in the vector and takes linear time.  The downside is that slightly more memory will be used to store a LazyController object tracking the sequence of fmaps that have occurred.
data MVector s a = MVector 
    { mvecAny :: !(VM.MVector s Any)
    , mvecInt :: !(VUM.MVector s Int)
    , mcontrol :: !LazyController
    }

data Vector a = Vector
    { vecAny :: !(V.Vector Any)
    , vecInt :: !(VU.Vector Int)
    , control :: !LazyController
    }

instance (Show a) => Show (Vector a) where
    show v = "fromList [" ++ go (VG.length v-1)
        where
            go (-1) = ""
            go i = go (i-1) ++ show (v VG.! i) ++ "," 


instance VGM.MVector MVector a where
    basicLength (MVector va bi c) = VGM.basicLength va
    basicUnsafeNew len = do
        mvecAny <- VGM.basicUnsafeNew len
        mvecInt <- VGM.basicUnsafeNew len
        return $ MVector
            { mvecAny = mvecAny
            , mvecInt = mvecInt
            , mcontrol = mempty
            }
    basicUnsafeRead v i = do
        any <- VGM.basicUnsafeRead (mvecAny v) i
        count <- VGM.basicUnsafeRead (mvecInt v) i
        let val = unsafeCoerce any
        if (funcC $ mcontrol $ v) == count
            then return val
            else do
                let count' = funcC $ mcontrol $ v 
                let any' = appList any (take (funcC (mcontrol v) - count) (funcL $ mcontrol $ v)) :: a
                VGM.basicUnsafeWrite (mvecAny v) i (unsafeCoerce any')
                VGM.basicUnsafeWrite (mvecInt v) i (count')
                return any'
    basicUnsafeWrite v i a = do
        VGM.basicUnsafeWrite (mvecAny v) i (unsafeCoerce a)
        VGM.basicUnsafeWrite (mvecInt v) i (funcC $ mcontrol $ v)
--     --basicUnsafeSlice = error "Data.Vector.SuperLazy.MVector does not support basicUnsafeSlice"
    basicOverlaps = error "Data.Vector.SuperLazy.MVector does not support basicOverlaps"
    basicUnsafeSlice s t v = MVector
        { mvecAny = VGM.basicUnsafeSlice s t (mvecAny v)
        , mvecInt = VGM.basicUnsafeSlice s t (mvecInt v)
        , mcontrol = mcontrol v
        }

type instance VG.Mutable Vector = MVector

instance VG.Vector Vector a where
    basicUnsafeFreeze v = do
        frozenAny <- VG.basicUnsafeFreeze (mvecAny v)
        frozenInt <- VG.basicUnsafeFreeze (mvecInt v)
        return $ Vector frozenAny frozenInt (mcontrol v)
    basicUnsafeThaw v = do
        thawedAny <- VG.basicUnsafeThaw (vecAny v)
        thawedInt <- VG.basicUnsafeThaw (vecInt v)
        return $ MVector thawedAny thawedInt (control v)
    --basicLength v = VG.basicLength $ vec v
    basicLength v = VG.basicLength $ vecAny v
    basicUnsafeSlice s t v = Vector (VG.basicUnsafeSlice s t $ vecAny v) (VG.basicUnsafeSlice s t $ vecInt v)  (control v)
    basicUnsafeIndexM v i = do
        any <- VG.basicUnsafeIndexM (vecAny v) i
        count <- VG.basicUnsafeIndexM (vecInt v) i
        return $ appList any (take (funcC (control v) - count) (funcL $ control v))

instance Functor Vector where
    fmap f v = v { control = LazyController
        { funcL = (unsafeCoerce f):(funcL $ control v)
        , funcC = 1+(funcC $ control v)
        }}
