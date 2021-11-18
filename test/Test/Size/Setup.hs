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
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Infrastructure for testing

  These are standard definitions, and will definitely lead to quadratic core.
-------------------------------------------------------------------------------}

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

deriving instance All Show xs => Show (HList xs)
deriving instance All Eq   xs => Eq   (HList xs)

