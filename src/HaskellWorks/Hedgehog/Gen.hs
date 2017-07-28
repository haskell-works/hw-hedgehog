module HaskellWorks.Hedgehog.Gen
  ( vector
  ) where

import Hedgehog
import Foreign.Storable

import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

vector :: MonadGen m => Range Int -> m a -> m (DV.Vector a)
vector n g = DV.fromList <$> G.list n g

storableVector :: (Storable a, MonadGen m) => Range Int -> m a -> m (DVS.Vector a)
storableVector n g = DVS.fromList <$> G.list n g
