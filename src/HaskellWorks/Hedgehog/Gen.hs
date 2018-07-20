{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Hedgehog.Gen
  ( vector
  , storableVector
  , word8x8
  ) where

import Data.Bits
import Data.Word
import Foreign.Storable
import Hedgehog

import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

vector :: MonadGen m => Range Int -> m a -> m (DV.Vector a)
vector n g = DV.fromList <$> G.list n g

storableVector :: (Storable a, MonadGen m) => Range Int -> m a -> m (DVS.Vector a)
storableVector n g = DVS.fromList <$> G.list n g

word8x8 :: MonadGen m => m Word8 -> m Word64
word8x8 gen = do
  a :: Word64 <- fromIntegral <$> gen
  b :: Word64 <- fromIntegral <$> gen
  c :: Word64 <- fromIntegral <$> gen
  d :: Word64 <- fromIntegral <$> gen
  e :: Word64 <- fromIntegral <$> gen
  f :: Word64 <- fromIntegral <$> gen
  g :: Word64 <- fromIntegral <$> gen
  h :: Word64 <- fromIntegral <$> gen
  return $  (a `shiftL` 56) .|.
            (b `shiftL` 48) .|.
            (c `shiftL` 40) .|.
            (d `shiftL` 32) .|.
            (e `shiftL` 24) .|.
            (f `shiftL` 16) .|.
            (g `shiftL`  8) .|.
            (h `shiftL`  0)
