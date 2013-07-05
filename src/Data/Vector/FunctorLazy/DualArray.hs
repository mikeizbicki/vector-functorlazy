{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables,TypeFamilies,FlexibleInstances,MultiParamTypeClasses #-}

module Data.Vector.FunctorLazy.DualArray
    where

import Data.Monoid hiding (Any)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Vector.Unboxed.Deriving
import Data.Primitive.Array
import Data.Primitive.ByteArray

import Control.Monad.ST
import Control.Monad.Primitive
import Unsafe.Coerce
import System.IO.Unsafe
import GHC.Prim

import Debug.Trace
import Data.Vector.FunctorLazy.Common

-- | Has the same semantics as Data.Vector, but offers an alternative time/space tradeoff.  In particular, super lazy Vectors make all calls to fmap happen in constant time; in traditional lazy vectors, fmap must be applied to each element in the vector and takes linear time.  The downside is that slightly more memory will be used to store a LazyController object tracking the sequence of fmaps that have occurred.
data MVector s a = MVector 
    { mvecAny :: !(MutableArray s Any)
    , mvecInt :: !(MutableByteArray s)
    , mlen :: !Int
    , mcontrol :: !LazyController
    }

data Vector a = Vector
    { vecAny :: !(Array Any)
    , vecInt :: !(ByteArray)
    , len :: !Int
    , control :: !LazyController
    }

instance (Show a) => Show (Vector a) where
    show v = "fromList [" ++ go (VG.length v-1)
        where
            go (-1) = ""
            go i = go (i-1) ++ show (v VG.! i) ++ "," 

uninitialized :: a
uninitialized = error "Data.Vector.FunctorLazy: uninitialized element"

{-# NOINLINE forceElement #-}
forceElement :: MVector s a -> Int -> a
-- forceElement (MVector va vi len (LazyController fl fc)) i = undefined
forceElement (MVector va vi len (LazyController fl fc)) i = unsafePerformIO $ do
    any <- readArray (unsafeCoerce va) i
    count :: Int <- readByteArray (unsafeCoerce vi) i
    let count' = fc 
    let any' = appList any (take (fc - count) fl) :: a
    writeArray (unsafeCoerce va) i (unsafeCoerce any')
    writeByteArray (unsafeCoerce vi) i (count')
    return any'

instance VGM.MVector MVector a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeNew #-}
--     {-# INLINE basicUnsafeRead #-}
--     {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeSlice #-}
    basicLength (MVector va bi l c) = l
    basicUnsafeNew len = do
        mvecAny <- newArray len uninitialized
        mvecInt <- newByteArray (len*8)
        setByteArray mvecInt 0 len (0::Int)
        return $ MVector
            { mvecAny = mvecAny
            , mvecInt = mvecInt
            , mlen = len
            , mcontrol = mempty
            }
--     basicUnsafeRead (MVector va vi len (LazyController fl fc)) i = {-trace "basicUnsafeRead" $ -}do
--         any <- readArray va i
--         count :: Int <- readByteArray vi i
--         let val = unsafeCoerce any
--         if fc == count
--             then return val
--             else do
--                 let count' = fc 
--                 let any' = appList any (take (fc - count) fl) :: a
--                 writeArray va i (unsafeCoerce any')
--                 writeByteArray vi i (count')
--                 return any'
--     basicUnsafeWrite (MVector va vi len (LazyController fl fc)) i a = {-trace "basicUnsafeWrite" $-} do
--         writeArray va i (unsafeCoerce a)
--         writeByteArray vi i fc
    basicUnsafeRead (MVector va vi len (LazyController fl fc)) i = {-trace "basicUnsafeRead" $ -}do
--         any <- readArray va i
--         return $ unsafeCoerce any
        any <- readArray va i
        count :: Int <- readByteArray vi i
        let val = unsafeCoerce any
        if fc == count
            then return val
            else return $ forceElement (MVector va vi len (LazyController fl fc)) i -- do
--             else return undefined -- do
--                 let count' = fc 
--                 let any' = appList any (take (fc - count) fl) :: a
--                 writeArray va i (unsafeCoerce any')
--                 writeByteArray vi i (count')
--                 return any'
    basicUnsafeWrite (MVector va vi len (LazyController fl fc)) i a = {-trace "basicUnsafeWrite" $-} do
        writeArray va i (unsafeCoerce a)
        writeByteArray vi i fc
-- --     --basicUnsafeSlice = error "Data.Vector.SuperLazy.MVector does not support basicUnsafeSlice"
    basicOverlaps = error "Data.Vector.FunctorLazy.MVector: basicOverlaps not supported"
--     basicUnsafeSlice s t v = error "Data.Vector.FunctorLazy.Mvector: basicUnsafeSlice" 
    basicUnsafeSlice s len v = {-trace ("basicUnsafeSlice, s="++show s++",len="++show len) $ -}unsafePerformIO $ do
        v' :: MVector RealWorld a <- VGM.basicUnsafeNew len
        do_copy s v'
        return $ unsafeCoerce v' 
        where
            do_copy i dst 
                | i < s+len = do
                    x <- VGM.basicUnsafeRead (unsafeCoerce v :: MVector RealWorld a) (s+i)
                    VGM.basicUnsafeWrite dst i x
                    do_copy (i+1) dst
                | otherwise = return ()

--     basicClear s t v = error "Data.Vector.FunctorLazy.Mvector: basicClear" 
--     basicUnsafeGrow v n = error "Data.Vector.FunctorLazy.Mvector: basicUnsafeGrow" 
    basicUnsafeGrow v by = do
        v' <- VGM.basicUnsafeNew (n+by)
        VGM.basicUnsafeCopy ({-basicUnsafeSlice 0 n-} v') v
        return v'
        where
            n = VGM.basicLength v 

type instance VG.Mutable Vector = MVector

instance VG.Vector Vector a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
--     {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze v = do
        frozenAny <- unsafeFreezeArray (mvecAny v)
        frozenInt <- unsafeFreezeByteArray (mvecInt v)
        return $ Vector frozenAny frozenInt (mlen v) (mcontrol v)
    basicUnsafeThaw v = do
        thawedAny <- unsafeThawArray (vecAny v)
        thawedInt <- unsafeThawByteArray (vecInt v)
        return $ MVector thawedAny thawedInt (len v) (control v)
--     --basicLength v = VG.basicLength $ vec v
    basicLength = len
--     basicUnsafeSlice s t v = Vector (VG.basicUnsafeSlice s t $ vecAny v) (VG.basicUnsafeSlice s t $ vecInt v)  (control v)
    basicUnsafeIndexM (Vector va vi len (LazyController fl fc)) i = do
        any <- indexArrayM va i
        let count = indexByteArray vi i
        let any' = unsafeCoerce any
        return $ appList any' (take (fc - count) fl)
    basicUnsafeSlice s len v = error "Data.Vector.FunctorLazy.Vector: slicing not supported"
    

instance Functor Vector where
    {-# INLINE fmap #-}
    fmap f v = v { control = LazyController
        { funcL = (unsafeCoerce f):(funcL $ control v)
        , funcC = 1+(funcC $ control v)
        }}

instance Functor (MVector s) where
    {-# INLINE fmap #-}
    fmap f v = v { mcontrol = LazyController
        { funcL = (unsafeCoerce f):(funcL $ mcontrol v)
        , funcC = 1+(funcC $ mcontrol v)
        }}
