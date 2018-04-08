module SMC.Util where

import Control.Monad.Logic

-- | Interleave all results from any foldable.
-- It's probably equivalent to msum . fmap return
bsum :: (MonadLogic m, Foldable t, Functor t) => t a -> m a
bsum = foldr interleave mzero . fmap return
