module TypeLet.Plugin (plugin) where

import Prelude hiding (cycle)

import Data.Traversable (forM)

import GhcPlugins hiding (substTy)
import TcEvidence
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
    case parseAll (parseLet rn) given of
      Left err ->
        errWith $ formatInvalidLet rn <$> err
      Right lets -> do
        case letsToSubst lets of
          Left cycle ->
            errWith $ formatLetCycle rn cycle
          Right subst -> do
            (solved, new) <- fmap unzip $
              forM (parseAll' (withOrig (parseEqual rn)) wanted) $
                uncurry (solveEqual subst)
            return $ TcPluginOk solved new
  where
    errWith :: GenLocated CtLoc PredType -> TcPluginM TcPluginResult
    errWith (L l err) = mkErr <$> newWanted' l err
      where
        mkErr :: CtEvidence -> TcPluginResult
        mkErr = TcPluginContradiction . (:[]) . mkNonCanonical

    -- Work-around bug in ghc, making sure the location is set correctly
    newWanted' :: CtLoc -> PredType -> TcPluginM CtEvidence
    newWanted' l w = setCtLocM' l $ newWanted l w

    -- Solve an Equal constraint by applying the substitution and turning it
    -- into a nominal equality constraint
    solveEqual ::
         TCvSubst
      -> Ct                       -- Original Equal constraint
      -> GenLocated CtLoc CEqual  -- Parsed Equal constraint
      -> TcPluginM ((EvTerm, Ct), Ct)
    solveEqual subst orig (L l parsed) = do
        ev <- newWanted l $
                mkPrimEqPredRole
                  Nominal
                  (substTy subst (equalLHS parsed))
                  (substTy subst (equalRHS parsed))
        return (
            (evidenceEqual rn parsed, orig)
          , mkNonCanonical ev
          )


