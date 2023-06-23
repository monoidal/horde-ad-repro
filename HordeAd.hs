{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module HordeAd where

import Prelude
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits

type family Element (x :: Type) :: Type
type instance Element x = Double
type Redundant a = (Element a ~ Double, Element a ~ Double)

data Array n = Arr Int deriving Show

data SizedList (n :: Nat) where
  Z :: SizedList 0
  C :: SizedList n -> SizedList (1 + n)

deriving instance Show (SizedList n)

rev :: forall a advals.
       ( Array 0 ~ advals
       , AdaptableInputs advals)
    => a -> Double -> ()
rev _ _ = unit @(Array 0) (toDomains @advals undefined)

unit :: Redundant a => a -> ()
unit _ = ()
{-# NOINLINE unit #-}

class AdaptableInputs advals where
  toDomains :: Redundant advals => advals -> advals
  unused :: advals -> advals

instance AdaptableInputs (Array n) where
  toDomains x = x
  unused x = x

tbuild1R :: forall n. KnownNat n
         => Int -> (() -> Array n) -> Array (1 + n)
tbuild1R k f = case natVal @n Proxy of
                  0 -> seq (f ()) (Arr (k + 1))
                  n -> error ("bug: " ++ show n)

buildSh :: forall m n. (KnownNat m, KnownNat n)
        => SizedList m -> Array n -> Array (m + n)
buildSh Z f = f
buildSh (C sh) f = tbuild1R 0 (\_ -> buildSh sh f)
