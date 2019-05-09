module Data.AStoreSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.State
import Data.AStore


foldStore :: forall a b m . (Monad m, AStore m a) => (b -> a -> b) -> b -> [a] -> m b
foldStore f b xs = do
  store <- initAStoreCommit @m @a
  _ <- traverse (commit store) xs
  foldAStore f b

spec :: Spec
spec = describe "AStore Spec - The Action Store" $ do
  describe "AStore (State [a]) a" $ do
    it "should store and retrive by accumulation any list of integers" $ property $ \(xs :: [Int]) 
       -> foldr (+) 0 xs == evalState (foldStore (+) 0 xs) ([] :: [Int])
    it "should store and retrive by accumulation any list of strings" $ property $ \(xs :: [String]) 
       -> foldr (++) "" xs == evalState (foldStore (++) "" xs) ([""] :: [String])
         

