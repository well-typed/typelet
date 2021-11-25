{-# LANGUAGE OverloadedStrings #-}

module TypeLet.Plugin.Constraints (
    -- * Constraints recognized by the plugin
    CEqual(..)
  , CLet(..)
    -- * Parsing
    -- ** Infrastructure
  , ParseResult(..)
  , parseAll
  , parseAll'
    -- ** SPecific parsers
  , InvalidLet(..)
  , parseEqual
  , parseLet
    -- * Evidence construction
  , evidenceEqual
    -- * Formatting errors
  , formatInvalidLet
  ) where

import Data.Void

import Outputable
import TcRnTypes
import GhcPlugins
import TcEvidence

import TypeLet.Plugin.NameResolution
import TypeLet.Plugin.Errors

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
  Parsing infrastructure
-------------------------------------------------------------------------------}

data ParseResult e a =
    -- | Parse successful
    ParseOk a

    -- | Different constraint than we're looking for (does not imply an error)
  | ParseNoMatch

    -- | Constraint of the shape we're looking for, but something is wrong
  | ParseError e

parseAll :: forall e a b. (a -> ParseResult e b) -> [a] -> Either (a, e) [(a, b)]
parseAll f = go []
  where
    go :: [(a, b)] -> [a] -> Either (a, e) [(a, b)]
    go acc []     = Right (reverse acc)
    go acc (a:as) = case f a of
                      ParseOk b    -> go ((a, b):acc) as
                      ParseNoMatch -> go         acc  as
                      ParseError e -> Left (a, e)

-- | Variation on 'parseAll' which rules out the error case
parseAll' :: (a -> ParseResult Void b) -> [a] -> [(a, b)]
parseAll' f = aux . parseAll f
  where
    aux :: Either (a, Void) [b] -> [b]
    aux (Left (_, v)) = absurd v
    aux (Right bs)    = bs

{-------------------------------------------------------------------------------
  Parser for specific constraints

  We can assume here that the constraint is kind correct, so if the class
  matches, we know how many arguments
-------------------------------------------------------------------------------}

data InvalidLet =
    -- | LHS should always be a variable
    NonVariableLHS Type Type Type

parseLet :: ResolvedNames -> Ct -> ParseResult InvalidLet CLet
parseLet ResolvedNames{..} ct =
    case classifyPredType (ctPred ct) of
      ClassPred cls [k, a, b] | cls == clsLet ->
        case getTyVar_maybe a of
          Nothing -> ParseError $ NonVariableLHS k a b
          Just x  -> ParseOk $ CLet k x b
      _otherwise ->
        ParseNoMatch

-- | Parse 'Equal' constraints
--
-- Kind-correct 'Equal' constraints of any form are ok, so this cannot return
-- errors.
parseEqual :: ResolvedNames -> Ct -> ParseResult Void CEqual
parseEqual ResolvedNames{..} ct =
    case classifyPredType (ctPred ct) of
      ClassPred cls [k, a, b] | cls == clsEqual ->
        ParseOk $ CEqual k a b
      _otherwise ->
        ParseNoMatch

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

{-------------------------------------------------------------------------------
  Formatting errors
-------------------------------------------------------------------------------}

formatInvalidLet :: ResolvedNames -> InvalidLet -> PredType
formatInvalidLet rn = mkTcPluginErrorTy rn . \case
    NonVariableLHS _k a b ->
         "Let with non-variable LHS:"
     :-: "Let " :|: PrintType a :|: " " :|: PrintType b