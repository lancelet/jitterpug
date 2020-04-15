{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Jitterpug.Sample
    ( SampleOps(SampleOps, sampleOpsScale, sampleOpsAdd)
    , cmj
    , permute
    , randFloat
    )
where

import           Control.Monad                  ( when )
import           Control.Monad.Primitive        ( PrimMonad
                                                , PrimState
                                                )
import           Control.Monad.ST               ( runST )
import           Data.Bits                      ( shiftR
                                                , xor
                                                , (.&.)
                                                , (.|.)
                                                )
import           Data.Primitive.MutVar          ( MutVar
                                                , modifyMutVar'
                                                , newMutVar
                                                , readMutVar
                                                )
import           Data.Word                      ( Word32 )
import           Prelude                        ( Bool
                                                , Float
                                                , Int
                                                , Monad
                                                , fromIntegral
                                                , mod
                                                , pure
                                                , quotRem
                                                , ($)
                                                , (*)
                                                , (+)
                                                , (-)
                                                , (/)
                                                , (>=)
                                                , (>>=)
                                                )

import           Jitterpug.Geom                 ( V2(V2) )

-- | A vector space for samples.
data SampleOps s v
  = SampleOps
    { sampleOpsScale :: s -> v -> v
    , sampleOpsAdd   :: v -> v -> v
    }

cmj :: Word32 -> Word32 -> Word32 -> Word32 -> V2 Float
cmj s m n p =
    let sdivm, smodm :: Word32
        (sdivm, smodm) = s `quotRem` m

        sx, sy :: Float
        sx = fromIntegral $ permute smodm m (p * 0xa511e9b3)
        sy = fromIntegral $ permute sdivm n (p * 0x63d83595)

        nf, mf :: Float
        nf = fromIntegral n
        mf = fromIntegral m

        jx, jy :: Float
        jx = randFloat s (p * 0xa399d265)
        jy = randFloat s (p * 0x711ad6a5)

        x, y :: Float
        x = (fromIntegral smodm + (sy + jx) / nf) / mf
        y = (fromIntegral sdivm + (sx + jy) / mf) / nf
    in  V2 x y

randFloat :: Word32 -> Word32 -> Float
randFloat i p = runST $ randFloatST i p

randFloatST
    :: forall m s
     . (PrimMonad m, s ~ PrimState m)
    => Word32
    -> Word32
    -> m Float
randFloatST ii pp = do
    im <- newMutVar ii
    im ^= pp
    with im $ \i -> im ^= i >> 17
    with im $ \i -> im ^= i >> 10
    im *= 0xb36534e5
    with im $ \i -> im ^= i >> 12
    with im $ \i -> im ^= i >> 21
    im *= 0x93fc4795
    im ^= 0xdf6e307f
    with im $ \i -> im ^= i >> 17
    im *= (1 .|. (pp >> 18))
    with im $ \i -> pure $ fromIntegral i / 4294967808

-- | Compute a pseudo-random permutation of indices.
permute
    :: Word32  -- ^ i - index into the permutation vector
    -> Word32  -- ^ l - length of permutation vector
    -> Word32  -- ^ p - pattern number (0 to l)
    -> Word32
permute i l p = runST (permuteST i l p)

permuteST
    :: forall m s
     . (PrimMonad m, s ~ PrimState m)
    => Word32
    -> Word32
    -> Word32
    -> m Word32
permuteST ii ll pp = do
    wm <- newMutVar $ ll - 1
    im <- newMutVar ii
    pm <- newMutVar pp

    with wm $ \w -> wm |= w >> 1
    with wm $ \w -> wm |= w >> 2
    with wm $ \w -> wm |= w >> 4
    with wm $ \w -> wm |= w >> 8
    with wm $ \w -> wm |= w >> 16

    with wm $ \w -> with pm $ \p ->
        doWhile (with im $ \i -> pure (i >= ll)) $ do
            im ^= p
            im *= 0xe170893d
            im ^= (p >> 16)
            with im $ \i -> im ^= (i .&. w) >> 4
            im ^= p >> 8
            im *= 0x0929eb3f
            im ^= p >> 23
            with im $ \i -> im ^= (i .&. w) >> 1
            im *= (1 .|. (p >> 27))
            im *= 0x6935fa69
            with im $ \i -> im ^= (i .&. w) >> 11
            im *= 0x74dcb303
            with im $ \i -> im ^= (i .&. w) >> 2
            im *= 0x9e501cc3
            with im $ \i -> im ^= (i .&. w) >> 2
            im *= 0xc860a3df
            im &= w
            with im $ \i -> im ^= i >> 5

    with im $ \i -> with pm $ \p -> pure $ (i + p) `mod` ll

(>>) :: Word32 -> Int -> Word32
(>>) = shiftR

(|=) :: (PrimMonad m, s ~ PrimState m) => MutVar s Word32 -> Word32 -> m ()
(|=) v x = modifyMutVar' v (.|. x)
infixl 4 |=

(^=) :: (PrimMonad m, s ~ PrimState m) => MutVar s Word32 -> Word32 -> m ()
(^=) v x = modifyMutVar' v (xor x)
infixl 4 ^=

(&=) :: (PrimMonad m, s ~ PrimState m) => MutVar s Word32 -> Word32 -> m ()
(&=) v x = modifyMutVar' v (.&. x)
infixl 4 &=

(*=) :: (PrimMonad m, s ~ PrimState m) => MutVar s Word32 -> Word32 -> m ()
(*=) v x = modifyMutVar' v (* x)

with :: (PrimMonad m, s ~ PrimState m) => MutVar s a -> (a -> m b) -> m b
with v op = readMutVar v >>= op

doWhile
    :: (Monad m)
    => m Bool  -- ^ Terminate when this returns False.
    -> m ()    -- ^ Monadic action to perform each loop (done at least once).
    -> m ()    -- ^ Overall monadic action.
doWhile while body = do
    body
    continue <- while
    when continue (doWhile while body)
