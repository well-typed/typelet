module Test.Size.HList.Setup (
    HList(..)
  , T(..)
  ) where

import Data.Kind
import GHC.TypeLits
import GHC.OverloadedLabels

data HList :: [Type] -> Type where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

data T :: Symbol -> Type where
  MkT :: T s

instance x ~ x' => IsLabel x (T x') where
  fromLabel = MkT

