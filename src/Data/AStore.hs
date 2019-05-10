module Data.AStore where

import Prelude hiding (lines)
import Conduit
import Control.Concurrent
import Control.Concurrent.STM.TBMChan
import Control.Monad.State
import Control.Monad.STM
import Data.Text (pack, unpack)
import Data.Conduit.Text (encode, decode, utf8, lines)
import Data.Conduit.TMChan (sourceTBMChan)
import Data.List (reverse)

data Commit m a where
  Commit :: Monad m => (a -> m ()) -> Commit m a

unCommit :: Commit m a -> (a -> m ())
unCommit (Commit f) = f

class Monad m => AStore m a where
  initAStoreCommit :: m (Commit m a)
  foldAStore       :: forall b . (b -> a -> b) -> b -> m b 

instance AStore (State [a]) a where
  initAStoreCommit = return $ Commit (\a -> modify (a :))
  foldAStore f b = get >>= return . foldl f b . reverse

instance forall a . (Show a, Read a) => AStore IO a where
  initAStoreCommit = do
    chan <- atomically $ newTBMChan 1000 
    _ <- forkIO $ runConduitRes
                $ sourceTBMChan chan
                .| mapC (pack . show)
                .| encode utf8
                .| sinkFile "~/.ordning_history"
    return $ Commit $ atomically . writeTBMChan chan
  foldAStore f b = runConduitRes 
               $ sourceFile "~/.ordning_history"
               .| decode utf8
               .| lines
               .| mapC (read . unpack)
               .| foldlC f b

