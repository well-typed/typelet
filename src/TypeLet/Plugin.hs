module TypeLet.Plugin (plugin) where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Traversable (forM)

import GhcPlugins hiding (substTy)
import TcPluginM
import TcRnTypes
import TyCoRep (substTy)

import TypeLet.Plugin.Constraints
import TypeLet.Plugin.NameResolution
import TypeLet.Plugin.Substitution
import TypeLet.Plugin.Util

{-------------------------------------------------------------------------------
  Top-level plumbing
-------------------------------------------------------------------------------}

plugin :: Plugin
plugin = defaultPlugin {
      pluginRecompile  = purePlugin
    , tcPlugin         = \_cmdline -> Just TcPlugin {
                             tcPluginInit  = resolveNames
                           , tcPluginSolve = solve
                           , tcPluginStop  = const $ return ()
                           }
    }

{-------------------------------------------------------------------------------
  Constraint resolution

  General approach: regard @Let@ constraints as defining a substitution, and
  then resolve @Equal@ constraints by /applying/ that substitution and
  simplifying to a derived equality constraint (derived instead of a new wanted
  constraint, because we don't actually need the evidence).
-------------------------------------------------------------------------------}

-- | Main interface to constraint resolution
--
-- NOTE: For now we are completely ignoring the derived constraints.
solve :: ResolvedNames -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve rn given derived wanted
    | null derived && null wanted = simplifyGivens  rn given
    | otherwise                   = simplifyWanteds rn given wanted

-- | Simplify givens
--
-- We (currently?) never simplify any givens, so we just two empty lists,
-- indicating that there no constraints were removed and none got added.
simplifyGivens :: ResolvedNames -> [Ct] -> TcPluginM TcPluginResult
simplifyGivens _st _given = return $ TcPluginOk [] []

-- | Simplify wanteds
--
-- This function provides the key functionality of the plugin.
--
-- We resolve 'Equal' constraints to /nominal/ equality constraints: we want
-- 'cast' to resolve @Let@ bindings, but not additionally work as 'coerce'.
simplifyWanteds :: ResolvedNames -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
simplifyWanteds rn@ResolvedNames{..} given wanted = do
    (solved, new) <- fmap (unzip . catMaybes) $ forM wanted $ \w ->
      case parseEqual rn w of
        Just w' -> do
          ev <- setCtLocM' (ctLoc w) $
                  newWanted (ctLoc w) $
                    mkPrimEqPredRole
                      Nominal
                      (substTy subst (equalLHS w'))
                      (substTy subst (equalRHS w'))
          return $ Just (
              (evidenceEqual rn w', w)
            , mkNonCanonical ev
            )
        Nothing ->
          return Nothing
    return $ TcPluginOk solved new
  where
    subst :: TCvSubst
    subst = letsToSubst $ mapMaybe (parseLet rn) given

