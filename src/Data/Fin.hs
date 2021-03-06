module Data.Fin
  ( SomeFin
  , Fin
  , fin
  , finLimit
  , finToInteger
  , predecessor
  , showNat
  ) where

import Data.Proxy
import GHC.TypeLits

data Fin (m :: Nat) where
  MkFin :: KnownNat m => Integer -> Fin m

data SomeFin where
  MkSomeFin :: KnownNat m => Fin m -> SomeFin

showNat :: KnownNat n => Proxy n -> String
showNat = show . natVal

instance Show (Fin s) where
  show (MkFin i) = "Fin " ++ showNat (Proxy @s) ++ ", value " ++ show i

instance Show SomeFin where
  show (MkSomeFin f) = "SomeFin of " ++ show f

instance KnownNat s => Bounded (Fin s) where
  minBound = MkFin 0
  maxBound = MkFin $ natVal (Proxy @s)

instance KnownNat s => Eq (Fin s) where
  (MkFin a) == (MkFin b) = a == b

predecessor :: Fin n -> Maybe (Fin n)
predecessor (MkFin 0) = Nothing
predecessor (MkFin n) = Just . MkFin $ n - 1

fin ::
     forall m. KnownNat m
  => Integer
  -> Maybe (Fin m)
fin n =
  if n <= natVal (Proxy @m)
    then Just $ MkFin n
    else Nothing

finLimit ::
     forall m. KnownNat m
  => Fin m
  -> Integer
finLimit _ = natVal (Proxy @m)

finToInteger :: Fin m -> Integer
finToInteger (MkFin i) = i
