-- | Thin layer around ghc-tcplugin-api
module TypeLet.Plugin.GhcTcPluginAPI (
    -- * Standard exports
    module GHC.TcPlugin.API
  , module Outputable
    -- * Additional re-exports
    -- ** Type variables
  , TyVar
  , TcType.isSkolemTyVar
    -- ** Substitutions
  , TCvSubst
  , TcType.substTy
  , TcType.substTyWith
  , TcType.tyCoVarsOfType
  , TcType.zipTvSubst
  , elemVarSet
    -- ** Location information
  , GenLocated(L)
  ) where

import GHC.TcPlugin.API
import Outputable

import SrcLoc (GenLocated(L))
import Type (TyVar)
import VarSet (elemVarSet)
import TcType (TCvSubst)

import qualified TcType
