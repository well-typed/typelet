module TypeLet.Plugin.Constraints (
    -- * Constraints recognized by the plugin
    CEqual(..)
  , CLet(..)
    -- * Parsing
  , parseEqual
  , parseLet
    -- * Evidence construction
  , evidenceEqual
  ) where

import Outputable
import TcRnTypes
import GhcPlugins
import TcEvidence

import TypeLet.Plugin.NameResolution

{-------------------------------------------------------------------------------
  Constraints recognized by the plugin
-------------------------------------------------------------------------------}

data CLet = CLet {
      letKind :: Type
    , letLHS  :: TyVar
    , letRHS  :: Type
    }

data CEqual = CEqual {
      equalKind :: Type
    , equalLHS  :: Type
    , equalRHS  :: Type
    }

instance Outputable CLet where
  ppr (CLet k a b) = parens $ text "Let" <+> ppr k <+> ppr a <+> ppr b

instance Outputable CEqual where
  ppr (CEqual k a b) = parens $ text "Equal" <+> ppr k <+> ppr a <+> ppr b

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseLet :: ResolvedNames -> Ct -> Maybe CLet
parseLet ResolvedNames{..} ct =
    case classifyPredType (ctPred ct) of
      ClassPred cls [k, a, b] | cls == clsLet ->
        case getTyVar_maybe a of
          Nothing -> panic $ "Let with non-variable LHS"
          Just x  -> Just $ CLet k x b
      _otherwise ->
        Nothing

parseEqual :: ResolvedNames -> Ct -> Maybe CEqual
parseEqual ResolvedNames{..} ct =
    case classifyPredType (ctPred ct) of
      ClassPred cls [k, a, b] | cls == clsEqual ->
        Just $ CEqual k a b
      _otherwise ->
        Nothing

{-------------------------------------------------------------------------------
  Evidence construction
-------------------------------------------------------------------------------}

-- | Evidence for an 'Equal' constraint
--
-- TODO: should we worry about producing an evidence term that prevents floating
-- stuff out of scope...? (the whole "coercions cannot simply be zapped" thing)
-- See also https://gitlab.haskell.org/ghc/ghc/-/issues/8095#note_108189 .
evidenceEqual :: ResolvedNames -> CEqual -> EvTerm
evidenceEqual ResolvedNames{..} (CEqual k a b) =
    evDataConApp
      (classDataCon clsEqual)
      [k, a, b]
      []
