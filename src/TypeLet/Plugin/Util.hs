module TypeLet.Plugin.Util (
    -- * Tracing
    tracePpr
    -- * GHC workarounds
  , setCtLocM'
  ) where

import DynFlags
import Outputable
import TcPluginM
import TcRnMonad

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

tracePpr :: forall a. Outputable a => Bool -> a -> TcPluginM ()
tracePpr debug = tcPluginIO . putStrLn . showSDoc' debug . ppr
  where
    showSDoc' :: Bool -> SDoc -> String
    showSDoc' True  = showSDocDebug unsafeGlobalDynFlags
    showSDoc' False = showSDoc      unsafeGlobalDynFlags

{-------------------------------------------------------------------------------
  GHC workarounds
-------------------------------------------------------------------------------}

-- | Wrapper around setCtLocM
--
-- For some reason just passing the location to 'newWanted' is unsufficient.
-- Possible bug in ghc?
setCtLocM' :: CtLoc -> TcPluginM a -> TcPluginM a
setCtLocM' l act = do
    xs <- getEvBindsTcPluginM
    unsafeTcPluginTcM (setCtLocM l $ runTcPluginM act xs)