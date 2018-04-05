module Util where

import Control.Monad.Logic

bsum :: (MonadLogic m, Foldable t, Functor t) => t a -> m a
bsum = foldr interleave mzero . fmap return
