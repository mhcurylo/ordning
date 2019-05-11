module Data.AStore where

import Conduit
import Control.Concurrent
import Control.Concurrent.STM.TBMChan
import Control.Monad.STM
import Control.Monad.State
import Data.Conduit.TMChan (sourceTBMChan)
import Data.Conduit.Text (decode, encode, lines, utf8)
import Data.List (reverse)
import Data.Text (pack, unpack)
import Prelude hiding (lines)

data AStoreHandler m a where
  AStoreHandler :: Monad m => (a -> m ()) -> m () -> AStoreHandler m a

commit :: AStoreHandler m a -> (a -> m ())
commit (AStoreHandler f _) = f

close :: AStoreHandler m a -> m ()
close (AStoreHandler _ c) = c

class Monad m =>
      AStore m a
  where
  initAStore :: String -> m (AStoreHandler m a)
  foldAStore :: forall b. String -> (b -> a -> b) -> b -> m b

instance AStore (State [a]) a where
  initAStore _ = return $ AStoreHandler (\a -> modify (a :)) (return ())
  foldAStore _ f b = get >>= return . foldl f b . reverse

instance forall a. (Show a, Read a) => AStore IO a where
  initAStore filename = do
    chan <- atomically $ newTBMChan 1000
    semaphore <- newEmptyMVar
    _ <-
      forkIO $ do
        runConduitRes $
          sourceTBMChan chan .| mapC (pack . (++ "\n") . show) .| encode utf8 .|
          sinkFile filename
        putMVar semaphore ()
    return $
      AStoreHandler (atomically . writeTBMChan chan) $ do
        atomically $ closeTBMChan chan
        takeMVar semaphore
  foldAStore filename f b = do
    res <-
      runConduitRes $
      sourceFile filename .| decode utf8 .| lines .| mapC (read . unpack) .|
      foldlC f b
    return res
