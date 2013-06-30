module Data.Vector.SuperLazy.Simple
    where

import qualified Data.Vector as V
import GHC.Prim
import Unsafe.Coerce

data Vector a = Vector
    { vec :: V.Vector Any 
    , controller :: LazyController
    }

-- | Records the sequence of fmaps that have occurred 
data LazyController = LazyController 
    { funcL :: [Any]
    , funcC :: Int
    }

instance Monoid LazyController where
    mempty = LazyController [] 0
    mappend a b = LazyController
        { funcL = funcL a ++ funcL b
        , funcC = funcC a + funcC b
        }

appList :: Any -> [Any] -> a
appList box xs = foldr (\f a -> (unsafeCoerce f) a) (unsafeCoerce box) xs


