{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Graphics.Jitterpug.CMJS (
    -- * Functions
    canonicalPixelSamples
    ) where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as VGM
import           Linear.V2                   (V2)
import qualified Linear.V2                   as V2


-- | Produce jittered pixel samples canonically.
--
--   This process produces pixel samples in the pattern shown in
--   Fig 1 of:
--
--     * Kensler, A (2013) Correlated Multi-Jittered Sampling.
--         Pixar Technical Memo 13-01.
--
--   > canonicalPixelSamples rand xs n
--
--   This is an effectful process, run in some primitive monad @m@. The
--   process modifies the mutable vector @xs@, filling it with sampling
--   locations. The square root of the total number of samples, @n@, is
--   provided as a convenience, but must match the length of the mutable
--   vector @xs@.
--
--   This procedure is typically called in a tight inner loop of the
--   sampling routine.
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
        let fj = fromIntegral j
        loop 0 (<n) (+1) $ \i -> do
            let fi = fromIntegral i
            xjit <- rand
            yjit <- rand
            VGM.unsafeWrite
                xs
                (j * n + i)
                (V2.V2
                    ((fi + (fj + xjit) / nf) / nf)
                    ((fj + (fi + yjit) / nf) / nf))
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
