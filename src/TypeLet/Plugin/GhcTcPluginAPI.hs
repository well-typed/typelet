{-# LANGUAGE CPP #-}

-- | Thin layer around ghc-tcplugin-api
module TypeLet.Plugin.GhcTcPluginAPI (
    -- * Standard exports
    module GHC.TcPlugin.API
  , module GHC.Utils.Outputable

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
    -- ** Errors
  , Panic.panic
  ) where

import GHC.TcPlugin.API

#if MIN_VERSION_ghc(9,0,0)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Subst (TCvSubst)
import GHC.Tc.Utils.TcType (isSkolemTyVar)
import GHC.Types.SrcLoc (GenLocated(L))
import GHC.Types.Var (TyVar)
import GHC.Types.Var.Set (elemVarSet)

import qualified GHC.Core.TyCo.Subst as Subst
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Outputable
import qualified GHC.Utils.Panic as Panic
#else
-- for ghc < 9.0, needs ghc-tcplugin-api's re-export of GHC.Utils.Outputable
import GHC.Utils.Outputable hiding (panic)
import qualified GHC.Utils.Outputable as Panic
#endif

#if !MIN_VERSION_ghc(9,0,0)

import SrcLoc (GenLocated(L))
import Type (TyVar)
import VarSet (elemVarSet)
import TcType (TCvSubst, tyCoVarsOfType, isSkolemTyVar)

import qualified TcType as Subst

#endif

