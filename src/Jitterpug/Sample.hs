{-# LANGUAGE TypeFamilies #-}
module Jitterpug.Sample
    ( SampleOps(SampleOps, sampleOpsScale, sampleOpsAdd)
    )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Primitive.MutVar (MutVar, newMutVar, modifyMutVar')
import Data.Bits ((.|.), shiftR, xor)
import Data.Word (Word32)

-- | A vector space for samples.
data SampleOps s v
  = SampleOps
    { sampleOpsScale :: s -> v -> v
    , sampleOpsAdd   :: v -> v -> v
    }


permute
  :: Word32  -- ^ i - index
  -> Word32  -- ^ l - length of permutation vector
  -> Word32  -- ^ p - pattern number
  -> Word32
permute i l p = runST $ do
  w <- newMutVar $ l - 1
  w `orEqShiftR` 1
  w `orEqShiftR` 2
  w `orEqShiftR` 4
  w `orEqShiftR` 8
  w `orEqShiftR` 16
  
  undefined

-- | Or-equals with a right shift.
--
-- The operation
--
-- @
--   orEqShiftR w i
-- @
--
-- is the same as the C code:
--
-- @
--   w |= w >> i;
-- @
orEqShiftR
  :: (PrimMonad m, s ~ PrimState m)
  => MutVar s Word32  -- ^ w
  -> Int              -- ^ i
  -> m ()
orEqShiftR v i = modifyMutVar' v (\x -> x .|. shiftR x i)
{-# INLINE orEqShiftR #-}

xorEq
  :: (PrimMonad m, s ~ PrimState m)
  => MutVar s Word32  -- ^ w
  -> Word32           -- ^ p
  -> m ()
xorEq w p = modifyMutVar' w (xor p)
