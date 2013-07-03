{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables,TypeFamilies,FlexibleInstances,MultiParamTypeClasses #-}

module Data.Vector.FunctorLazy.Common
    where

import GHC.Prim
import Unsafe.Coerce
import Data.Monoid hiding (Any)

-- | Every position in the super lazy vector is represented by a LazyBox
data LazyBox = LazyBox 
    { lazyc :: !Int -- ^ how many functions have been applied to this box
    , lazyb :: Any -- ^ current partially evaluated thunk
    }

mkLazyBox :: a -> LazyBox
mkLazyBox a = LazyBox
    { lazyc = 0
    , lazyb = unsafeCoerce a
    }

-- | Records the sequence of fmaps that have occurred 
data LazyController = LazyController 
    { funcL :: [Any]
    , funcC :: {-# UNBOX #-} !Int
    }

instance Monoid LazyController where
    mempty = LazyController [] 0
    mappend a b = LazyController
        { funcL = funcL a ++ funcL b
        , funcC = funcC a + funcC b
        }

appList :: Any -> [Any] -> a
appList box xs = foldr (\f a -> (unsafeCoerce f) a) (unsafeCoerce box) xs


