module TypeLet.Plugin.NameResolution (
    ResolvedNames(..)
  , resolveNames
  ) where

import GHC
import GhcPlugins
import TcPluginM

data ResolvedNames = ResolvedNames {
      clsEqual :: Class
    , clsLet   :: Class
    }

instance Outputable ResolvedNames where
  ppr ResolvedNames{..} = vcat [
        text "ResolvedNames {"
      , nest 2 $ vcat [
            text "clsEqual =" <+> ppr clsEqual
          , text "clsLet   =" <+> ppr clsLet
          ]
      , text "}"
      ]

resolveNames :: TcPluginM ResolvedNames
resolveNames = do
    m <- do r <- findImportedModule typeletMod (Just typeletPkg)
            case r of
              Found _ m  -> return m
              _otherwise -> panic $ "Could not find "
                                 ++ showSDocUnsafe (ppr typeletMod)

    clsEqual <- tcLookupClass =<< lookupOrig m (mkTcOcc "Equal")
    clsLet   <- tcLookupClass =<< lookupOrig m (mkTcOcc "Let")

    return ResolvedNames{..}
  where
    typeletMod :: ModuleName
    typeletMod = mkModuleName "TypeLet.UserAPI"

    typeletPkg :: FastString
    typeletPkg = fsLit "typelet"

