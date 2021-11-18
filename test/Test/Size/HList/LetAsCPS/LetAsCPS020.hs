{-# LANGUAGE CPP #-}

#if defined(USE_GHC_DUMP)
{-# OPTIONS_GHC -fplugin=GhcDump.Plugin #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Test.Size.HList.LetAsCPS.LetAsCPS020 where

import TypeLet

import Test.Size.HList.Setup
import Test.Size.HList.Index.Ix020

hlist :: HList Fields
hlist = letT' (Proxy @Fields) $ \(_ :: Proxy r) -> castEqual $
    -- 19 .. 10
    letAs' @(HList r) (HCons (MkT @"i19") HNil) $ \(xs19 :: HList t19) ->
    letAs' @(HList r) (HCons (MkT @"i18") xs19) $ \(xs18 :: HList t18) ->
    letAs' @(HList r) (HCons (MkT @"i17") xs18) $ \(xs17 :: HList t17) ->
    letAs' @(HList r) (HCons (MkT @"i16") xs17) $ \(xs16 :: HList t16) ->
    letAs' @(HList r) (HCons (MkT @"i15") xs16) $ \(xs15 :: HList t15) ->
    letAs' @(HList r) (HCons (MkT @"i14") xs15) $ \(xs14 :: HList t14) ->
    letAs' @(HList r) (HCons (MkT @"i13") xs14) $ \(xs13 :: HList t13) ->
    letAs' @(HList r) (HCons (MkT @"i12") xs13) $ \(xs12 :: HList t12) ->
    letAs' @(HList r) (HCons (MkT @"i11") xs12) $ \(xs11 :: HList t11) ->
    letAs' @(HList r) (HCons (MkT @"i10") xs11) $ \(xs10 :: HList t10) ->
    -- 09 .. 00
    letAs' @(HList r) (HCons (MkT @"i09") xs10) $ \(xs09 :: HList t09) ->
    letAs' @(HList r) (HCons (MkT @"i08") xs09) $ \(xs08 :: HList t08) ->
    letAs' @(HList r) (HCons (MkT @"i07") xs08) $ \(xs07 :: HList t07) ->
    letAs' @(HList r) (HCons (MkT @"i06") xs07) $ \(xs06 :: HList t06) ->
    letAs' @(HList r) (HCons (MkT @"i05") xs06) $ \(xs05 :: HList t05) ->
    letAs' @(HList r) (HCons (MkT @"i04") xs05) $ \(xs04 :: HList t04) ->
    letAs' @(HList r) (HCons (MkT @"i03") xs04) $ \(xs03 :: HList t03) ->
    letAs' @(HList r) (HCons (MkT @"i02") xs03) $ \(xs02 :: HList t02) ->
    letAs' @(HList r) (HCons (MkT @"i01") xs02) $ \(xs01 :: HList t01) ->
    letAs' @(HList r) (HCons (MkT @"i00") xs01) $ \(xs00 :: HList t00) ->
      castEqual xs00
