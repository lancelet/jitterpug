{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Graphics.Jitterpug.CMJS where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as VGM
import           Linear.V2                   (V2)
import qualified Linear.V2                   as V2
import           System.Random.MWC           (Gen)

-- | Arrange jittered pixel samples canonically.
canonicalPixelSamples
    :: ( PrimMonad m
       , MVector v (V2 a)
       , Num a
       , Fractional a )
    => m a                     -- ^ Random number generator.
    -> v (PrimState m) (V2 a)  -- ^ Multable vector for the pixel samples.
    -> Int                     -- ^ Square root of vector length.
    -> m ()
canonicalPixelSamples !rand !xs !n = do
    let nf = fromIntegral n
    loop 0 (<n) (+1) $ \j -> do
        let jf = fromIntegral j
        loop 0 (<n) (+1) $ \i -> do
            xjit <- rand
            yjit <- rand
            VGM.unsafeWrite
                xs
                (i + (j * n))
                (V2.V2
                    ((fromIntegral i + xjit / nf) / nf)
                    ((jf             + yjit / nf) / nf))
{-# INLINE canonicalPixelSamples #-}

-- | Basic for loop.
loop
    :: (Monad m)
    => a            -- ^ Starting value of the loop.
    -> (a -> Bool)  -- ^ Terminate when this function returns False.
    -> (a -> a)     -- ^ State transition / step function.
    -> (a -> m ())  -- ^ Body of the loop.
    -> m ()
loop start while step body = go start
  where
    go !i | while i   = body i >> go (step i)
          | otherwise = return ()
{-# INLINE loop #-}
