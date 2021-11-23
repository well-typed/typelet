-- | Custom type errors
--
-- This is adapted from the @ghc-tcplugin-api@ library.
module TypeLet.Plugin.Errors (
    TcPluginErrorMessage(..)
  , mkTcPluginErrorTy
  ) where

import Data.String

import GhcPlugins
import TyCoRep

import TypeLet.Plugin.NameResolution

-- | Custom error message
--
-- See 'interpretErrorMessage'
data TcPluginErrorMessage =
    -- | Show the text as is
    Txt String

    -- | Pretty print the given type
  | PrintType Type

    -- | Put two messages side by side
  | (:|:) TcPluginErrorMessage TcPluginErrorMessage

    -- | Stack two messages vertically.
  | (:-:) TcPluginErrorMessage TcPluginErrorMessage

instance IsString TcPluginErrorMessage where
  fromString = Txt

infixl 5 :|:
infixl 6 :-:

-- | Construct a custom type error (the equivalent of user-land @TypeError@)
mkTcPluginErrorTy :: ResolvedNames -> TcPluginErrorMessage -> PredType
mkTcPluginErrorTy ResolvedNames{..} = mkTypeErr . interpret
  where
    interpret :: TcPluginErrorMessage -> PredType
    interpret (Txt str)       = mkTyConApp tyConText   [LitTy . StrTyLit . fsLit $ str]
    interpret (PrintType ty)  = mkTyConApp tyConShow   [tcTypeKind ty, ty]
    interpret (msg1 :|: msg2) = mkTyConApp tyConConcat [interpret msg1, interpret msg2]
    interpret (msg1 :-: msg2) = mkTyConApp tyConVCat   [interpret msg1, interpret msg2]

    mkTypeErr :: PredType -> PredType
    mkTypeErr err = mkTyConApp tyConTypeErr [constraintKind, err]


