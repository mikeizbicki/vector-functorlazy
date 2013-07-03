{-# LANGUAGE MagicHash #-}

import Criterion.Config
import Criterion.Main
import GHC.Prim

myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 5
    }

-- mkArray :: Int# -> IO ()
mkArray i = do
    (# _dum, arr #) <- newArray# 10 (undefined::Int) (undefined::RealWorld)
    putStrLn "Done."
    return ()

main = defaultMainWith myConfig (return ())
    [ bench "array#" $ mkArray 10 
    ]
--     [ bench "V.Vector-fmap-1000" $ whnf (fmap (+1)) $ vector1000
--     , bench "V.Vector-fmap-10000" $ whnf (fmap (+1)) $ vector10000
--     , bench "V.Vector-fmap-100000" $ whnf (fmap (+1)) $ vector100000
--     , bench "V.Vector-fmap-1000000" $ whnf (fmap (+1)) $ vector1000000
--     ]
