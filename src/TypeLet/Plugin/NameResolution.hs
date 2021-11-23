module TypeLet.Plugin.NameResolution (
    ResolvedNames(..)
  , resolveNames
  ) where

import GHC
import GhcPlugins
import TcPluginM
import PrelNames

data ResolvedNames = ResolvedNames {
      clsEqual     :: Class
    , clsLet       :: Class
    , tyConTypeErr :: TyCon
    , tyConText    :: TyCon
    , tyConShow    :: TyCon
    , tyConConcat  :: TyCon
    , tyConVCat    :: TyCon
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

    -- Constraints handled by the plugin

    clsEqual <- tcLookupClass =<< lookupOrig m (mkTcOcc "Equal")
    clsLet   <- tcLookupClass =<< lookupOrig m (mkTcOcc "Let")

    -- Constructors for custom error messages
    -- (Adopted from @initBuiltinDefs@ in @ghc-tcplugin-api@)

    tyConTypeErr <-                    tcLookupTyCon   errorMessageTypeErrorFamName
    tyConText    <- promoteDataCon <$> tcLookupDataCon typeErrorTextDataConName
    tyConShow    <- promoteDataCon <$> tcLookupDataCon typeErrorShowTypeDataConName
    tyConConcat  <- promoteDataCon <$> tcLookupDataCon typeErrorAppendDataConName
    tyConVCat    <- promoteDataCon <$> tcLookupDataCon typeErrorVAppendDataConName

    return ResolvedNames{..}
  where
    typeletMod :: ModuleName
    typeletMod = mkModuleName "TypeLet.UserAPI"

    typeletPkg :: FastString
    typeletPkg = fsLit "typelet"

