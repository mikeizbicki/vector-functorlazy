import Criterion.Config
import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G

import qualified Data.Vector.SuperLazy as SLV

myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 5
    }

vector1000 = V.fromList [1..1000] :: V.Vector Int
vector10000 = V.fromList [1..10000] :: V.Vector Int
vector100000 = V.fromList [1..100000] :: V.Vector Int
vector1000000 = V.fromList [1..1000000] :: V.Vector Int

lv1000 = G.fromList [1..1000] :: SLV.Vector Int
lv10000 = G.fromList [1..10000] :: SLV.Vector Int
lv100000 = G.fromList [1..100000] :: SLV.Vector Int
lv1000000 = G.fromList [1..1000000] :: SLV.Vector Int

main = defaultMainWith myConfig (return ())
    [ bench "LazyVector-fmap-1000" $ whnf (fmap (+1)) $ lv1000
    , bench "LazyVector-fmap-10000" $ whnf (fmap (+1)) $ lv10000
    , bench "LazyVector-fmap-100000" $ whnf (fmap (+1)) $ lv100000
    , bench "LazyVector-fmap-1000000" $ whnf (fmap (+1)) $ lv1000000
    ]
--     [ bench "V.Vector-fmap-1000" $ whnf (fmap (+1)) $ vector1000
--     , bench "V.Vector-fmap-10000" $ whnf (fmap (+1)) $ vector10000
--     , bench "V.Vector-fmap-100000" $ whnf (fmap (+1)) $ vector100000
--     , bench "V.Vector-fmap-1000000" $ whnf (fmap (+1)) $ vector1000000
--     ]
