{-# LANGUAGE CPP #-}

-- | Thin layer around ghc-tcplugin-api
module TypeLet.Plugin.GhcTcPluginAPI (
    -- * Standard exports
    module GHC.TcPlugin.API

#if MIN_VERSION_ghc(9,0,0)
  , module GHC.Utils.Outputable
#else
  , module Outputable
#endif

    -- * Additional re-exports
    -- ** Type variables
  , TyVar
  , isSkolemTyVar
    -- ** Substitutions
  , TCvSubst
  , Subst.substTy
  , Subst.substTyWith
  , Subst.zipTvSubst
  , elemVarSet
  , tyCoVarsOfType
    -- ** Location information
  , GenLocated(L)
  ) where

import GHC.TcPlugin.API

#if MIN_VERSION_ghc(9,0,0)

import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Subst (TCvSubst)
import GHC.Tc.Utils.TcType (isSkolemTyVar)
import GHC.Types.SrcLoc (GenLocated(L))
import GHC.Types.Var (TyVar)
import GHC.Types.Var.Set (elemVarSet)
import GHC.Utils.Outputable

import qualified GHC.Core.TyCo.Subst as Subst

#else

import Outputable
import SrcLoc (GenLocated(L))
import Type (TyVar)
import VarSet (elemVarSet)
import TcType (TCvSubst)

import qualified TcType

#endif