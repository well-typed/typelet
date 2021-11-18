module Test.Size.Setup (
    HList(..)
  , T(..)
  ) where

import Data.Kind
import GHC.TypeLits

data HList :: [Type] -> Type where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

data T :: Symbol -> Type where
  MkT :: T s