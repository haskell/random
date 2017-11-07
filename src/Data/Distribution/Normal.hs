{-# LANGUAGE  ScopedTypeVariables #-}
module Data.Distribution.Normal where


import Numeric.Extras (hypot)
{- | For now this will be using the Marsaglia polar method,
though might be better methods to use in exchange for a teeny bit more complexity.

NB: tail distribution depends on quality of the unit interval generator

This implementation only returns one of the two random variates, and thus
only needs to generate real samples from [+0,1) rather than (-1,1)


if using the x / 2^-53 style uniform interval, you cant get a result r
such that abs(r) > 15, though thats pretty extreme in the tail distribution
-}
unitNormalPolarMethodM :: forall m . Monad m => m Double -> m Bool -> m Double
unitNormalPolarMethodM unitSampler boolSampler = getSample
  where
    getSample :: m Double
    getSample = do
      x <- unitSampler
      y <- unitSampler
      sqrtSumSq   <- return $ hypot x y
      straightSum <- return $ x*x + y*y
      if straightSum >= 1 || straightSum == 0
        --- the usual condition is  x^2 + y^2 > 1, but the same bound holds for the sqrt thereof
        then getSample
        else
          do
            boolS <- boolSampler
            signed <- return $ if boolS then 1 else -1
            return $! signed * x * (sqrt ( -2 * log straightSum) /  sqrtSumSq)


