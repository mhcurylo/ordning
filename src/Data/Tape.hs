module Data.Tape where

import Control.Comonad

infixr 8 :>
data Stream a = a :> Stream a

head :: Stream a -> a
head (x :> _) = x

tail :: Stream a -> Stream a
tail (_ :> xs) = xs

cons :: a -> Stream a -> Stream a
cons = (:>)

fromList :: [a] -> Stream a
fromList = fromInfiniList . cycle
  where
    fromInfiniList (y:ys) = y :> fromInfiniList ys
    fromInfiniList _ = error "This is an unsafe operation. Never use it on empty lists."

instance Show a => Show (Stream a) where
  show (x:>x':>_) = "Stream " ++ show x ++ ", " ++ show x' ++ ".."

instance Functor Stream where
  fmap f (x :> xs) = f x :> fmap f xs

iterateS :: (a -> a) -> a -> Stream a
iterateS f v = vs 
  where
  vs = v :> fmap f vs

instance Comonad Stream where
  extract = Data.Tape.head
  duplicate = iterateS Data.Tape.tail

data Tape a = Tape (Stream a) a (Stream a)

value :: Tape a -> a
value (Tape _ v _) = v
left :: Tape a -> Tape a
left (Tape (x:>xs) v ys) = Tape xs x (v:>ys)
right :: Tape a -> Tape a
right (Tape xs v (y:>ys)) = Tape (v:>xs) y ys

swapValue :: a -> Tape a -> Tape a
swapValue nv (Tape xs _ ys) = Tape xs nv ys

instance Show a => Show (Tape a) where
  show (Tape (x:>_) v (y:>_)) = "Tape .." ++ show x ++ ", " ++ show v ++ ", " ++ show y ++ ".."

instance Functor Tape where
  fmap f (Tape xs v ys) = Tape (fmap f xs) (f v) (fmap f ys)

instance Comonad Tape where
  extract = Data.Tape.value
  duplicate t = Tape (iterateS left (left t)) t (iterateS right (right t))

fromLists :: [a] -> [a] -> Tape a
fromLists xs ys = Tape (fromList xs) y ys'
  where
  (y:>ys') = fromList ys
