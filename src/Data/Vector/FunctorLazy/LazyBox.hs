{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables,TypeFamilies,FlexibleInstances,MultiParamTypeClasses #-}

module Data.Vector.FunctorLazy.LazyBox
    where

import Data.Monoid hiding (Any)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed.Deriving

import Control.Monad.ST
import Control.Monad.Primitive
import Unsafe.Coerce
import System.IO.Unsafe
import GHC.Prim

import Data.Vector.FunctorLazy.Common

-- | Has the same semantics as Data.Vector, but offers an alternative time/space tradeoff.  In particular, super lazy Vectors make all calls to fmap happen in constant time; in traditional lazy vectors, fmap must be applied to each element in the vector and takes linear time.  The downside is that slightly more memory will be used to store a LazyController object tracking the sequence of fmaps that have occurred.
data MVector s a = MVector 
    { mvec :: !(VM.MVector s LazyBox)
    , mcontrol :: !LazyController
    }

data Vector a = Vector
    { vec :: !(V.Vector LazyBox)
    , control :: !LazyController
    }

instance (Show a) => Show (Vector a) where
    show v = "fromList [" ++ go (VG.length v-1)
        where
            go (-1) = ""
            go i = go (i-1) ++ show (v VG.! i) ++ "," 

data VectorFail a = VectorFail
    { vec_fail :: VM.IOVector LazyBox
    , control_fail :: LazyController
    }

type instance VG.Mutable VectorFail = MVector

instance VGM.MVector MVector a where
    basicLength (MVector v c) = VGM.basicLength v
    basicUnsafeNew len = do
        mvec <- VGM.basicUnsafeNew len
        return $ MVector
            { mvec = mvec
            , mcontrol = mempty
            }
    basicUnsafeRead v i = do
        (LazyBox count any) <- VGM.basicUnsafeRead (mvec v) i
        let val = unsafeCoerce any
        if (funcC $ mcontrol $ v) == count
            then return val
            else do
                let count' = funcC $ mcontrol $ v 
                let any' = appList any (take (funcC (mcontrol v) - count) (funcL $ mcontrol $ v)) :: a
                VGM.basicUnsafeWrite (mvec v) i (LazyBox count' (unsafeCoerce any')) 
                return any'
    basicUnsafeWrite v i a =
        VGM.basicUnsafeWrite (mvec v) i $ LazyBox
            { lazyc = funcC $ mcontrol $ v
            , lazyb = unsafeCoerce a
            }
    --basicUnsafeSlice = error "Data.Vector.SuperLazy.MVector does not support basicUnsafeSlice"
    basicOverlaps = error "Data.Vector.SuperLazy.MVector does not support basicOverlaps"

    basicUnsafeSlice s t v = MVector
        { mvec = VGM.basicUnsafeSlice s t (mvec v)
        , mcontrol = mcontrol v
        }
    {-basicUnsafeSlice s t v = unsafePerformIO $ do
        v' <- VGM.new (t-s+1) --(mvec v)
        VGM.copy v' (VGM.slice s t $ mvec v)
        return $ MVector
            { mvec = v'
            , mcontrol = mcontrol v
            }
-}
type instance VG.Mutable Vector = MVector

instance VG.Vector Vector a where
    basicUnsafeFreeze v = do
        frozen <- VG.basicUnsafeFreeze (mvec v)
        return $ Vector frozen (mcontrol v)
    basicUnsafeThaw v = do
        thawed <- VG.basicUnsafeThaw (vec v)
        return $ MVector thawed (control v)
    --basicLength v = VG.basicLength $ vec v
    basicLength v = VG.basicLength $ vec v
    basicUnsafeSlice s t v = Vector (VG.basicUnsafeSlice s t $ vec v) (control v)
    basicUnsafeIndexM v i = do
        LazyBox count any <- VG.basicUnsafeIndexM (vec v) i
        return $ appList any (take (funcC (control v) - count) (funcL $ control v))

instance VG.Vector VectorFail a where
    basicUnsafeFreeze v = do
        (frozen::V.Vector LazyBox) <- VG.basicUnsafeFreeze (mvec v) 
        return $ unsafePerformIO $ do
            v' <- VG.basicUnsafeThaw frozen 
            return $ VectorFail v' (mcontrol v)
    basicUnsafeThaw v = do 
        thawed <- VG.basicUnsafeThaw $ (unsafePerformIO $ VG.basicUnsafeFreeze (vec_fail v) :: V.Vector LazyBox)
        return $ MVector
            { mvec = thawed 
            , mcontrol = control_fail v
            }
    basicLength v = VGM.basicLength (vec_fail v)


{-fromList :: [a] -> MVector a
fromList xs = MVector
    { vec_fail = VM.fromList $ map mkLazyBox xs
    , control_fail = LazyController [] 0
    }
-}    
instance Functor Vector where
    fmap f lv = lv { control = LazyController
        { funcL = (unsafeCoerce f):(funcL $ control lv)
        , funcC = 1+(funcC $ control lv)
        }}
{-
lvfind :: Vector a -> Int -> a
lvfind lv i = appList (lazyb $ vec_fail lv V.! i) (funcL $ control_fail lv)

appList' :: a -> [a->a] -> a
appList' = foldr (\f a -> f a)

appList :: Any -> [Any] -> a
appList box xs = foldr (\f a -> (unsafeCoerce f) a) (unsafeCoerce box) xs

boxx :: Any 
boxx = unsafeCoerce (10::Int)

boxf :: [Any]
boxf = 
   [ unsafeCoerce (show :: Int -> String)
   , unsafeCoerce (+1)
   , unsafeCoerce (*2)
   , unsafeCoerce (+1)
   ]
   -}
