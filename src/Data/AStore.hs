module Data.AStore where

import Conduit
import Control.Concurrent.STM.TBMChan
import Control.Monad.State
import Data.List (reverse)

newtype Commit m a = Commit { commit :: a -> m () }

class Monad m => AStore m a where
  initAStoreCommit :: m (Commit m a)
  foldAStore       :: forall b . (b -> a -> b) -> b -> m b 

instance AStore (State [a]) a where
  initAStoreCommit = return $ Commit (\a -> modify (a :))
  foldAStore f b = get >>= return . foldl f b . reverse



