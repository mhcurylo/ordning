module Data.AStoreSpec where

import Control.Arrow ((>>>))
import Control.Exception hiding (assert)
import Control.Monad.State
import Data.AStore
import Prelude hiding (catch)
import System.Directory
import System.IO.Error hiding (catch)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

testFile :: String
testFile = "./test_store"

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

foldStore ::
     forall a b m. (Monad m, AStore m a)
  => (b -> a -> b)
  -> b
  -> [a]
  -> m b
foldStore f b xs = do
  store <- (initAStore testFile)
  _ <- traverse (commit store) xs
  close store
  foldAStore testFile f b

foldIOStore ::
     forall a b. (AStore IO a)
  => (b -> a -> b)
  -> b
  -> [a]
  -> IO b
foldIOStore f b xs = removeIfExists testFile >> foldStore f b xs

spec :: Spec
spec =
  describe "AStore Spec - The Action Store" $ do
    describe "AStore (State [a]) a" $ do
      it "should store and retrive by accumulation any list of integers" $
        property $ \(xs :: [Int]) ->
          foldr (+) 0 xs == evalState (foldStore (+) 0 xs) ([] :: [Int])
      it "should store and retrive by accumulation any list of strings" $
        property $ \(xs :: [String]) ->
          foldr (++) "" xs ==
          evalState (foldStore (++) "" xs) ([""] :: [String])
    describe "AStore IO a" $ do
      it "should store and retrive by accumulation a list of integers" $
        property $ \(xs :: [Int]) ->
          monadicIO $
          run (foldIOStore (+) 0 xs) >>= \res -> assert $ res == foldr (+) 0 xs
      it "should store and retrive by accumulation any list of strings" $
        property $ \(xs :: [String]) ->
          monadicIO $
          run (foldIOStore (++) "" xs) >>= \res ->
            assert $ res == foldr (++) "" xs
