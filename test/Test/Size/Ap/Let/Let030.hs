{-# LANGUAGE CPP #-}

#if defined(USE_GHC_DUMP)
{-# OPTIONS_GHC -fplugin=GhcDump.Plugin #-}
#endif

{-# OPTIONS_GHC -fplugin=TypeLet #-}

module Test.Size.Ap.Let.Let030 where

import TypeLet

import Test.Size.Ap.Index.Ix030
import Test.Size.Setup

applyF :: forall f r. Applicative f => F r -> f r
applyF f =
    -- 29 .. 20
    case letT (Proxy @(T "i29" -> r))   of { LetT (_ :: Proxy l29) ->
    case letT (Proxy @(T "i28" -> l29)) of { LetT (_ :: Proxy l28) ->
    case letT (Proxy @(T "i27" -> l28)) of { LetT (_ :: Proxy l27) ->
    case letT (Proxy @(T "i26" -> l27)) of { LetT (_ :: Proxy l26) ->
    case letT (Proxy @(T "i25" -> l26)) of { LetT (_ :: Proxy l25) ->
    case letT (Proxy @(T "i24" -> l25)) of { LetT (_ :: Proxy l24) ->
    case letT (Proxy @(T "i23" -> l24)) of { LetT (_ :: Proxy l23) ->
    case letT (Proxy @(T "i22" -> l23)) of { LetT (_ :: Proxy l22) ->
    case letT (Proxy @(T "i21" -> l22)) of { LetT (_ :: Proxy l21) ->
    case letT (Proxy @(T "i20" -> l21)) of { LetT (_ :: Proxy l20) ->
    -- 19 .. 10
    case letT (Proxy @(T "i19" -> l20)) of { LetT (_ :: Proxy l19) ->
    case letT (Proxy @(T "i18" -> l19)) of { LetT (_ :: Proxy l18) ->
    case letT (Proxy @(T "i17" -> l18)) of { LetT (_ :: Proxy l17) ->
    case letT (Proxy @(T "i16" -> l17)) of { LetT (_ :: Proxy l16) ->
    case letT (Proxy @(T "i15" -> l16)) of { LetT (_ :: Proxy l15) ->
    case letT (Proxy @(T "i14" -> l15)) of { LetT (_ :: Proxy l14) ->
    case letT (Proxy @(T "i13" -> l14)) of { LetT (_ :: Proxy l13) ->
    case letT (Proxy @(T "i12" -> l13)) of { LetT (_ :: Proxy l12) ->
    case letT (Proxy @(T "i11" -> l12)) of { LetT (_ :: Proxy l11) ->
    case letT (Proxy @(T "i10" -> l11)) of { LetT (_ :: Proxy l10) ->
    -- 09 .. 00
    case letT (Proxy @(T "i09" -> l10)) of { LetT (_ :: Proxy l09) ->
    case letT (Proxy @(T "i08" -> l09)) of { LetT (_ :: Proxy l08) ->
    case letT (Proxy @(T "i07" -> l08)) of { LetT (_ :: Proxy l07) ->
    case letT (Proxy @(T "i06" -> l07)) of { LetT (_ :: Proxy l06) ->
    case letT (Proxy @(T "i05" -> l06)) of { LetT (_ :: Proxy l05) ->
    case letT (Proxy @(T "i04" -> l05)) of { LetT (_ :: Proxy l04) ->
    case letT (Proxy @(T "i03" -> l04)) of { LetT (_ :: Proxy l03) ->
    case letT (Proxy @(T "i02" -> l03)) of { LetT (_ :: Proxy l02) ->
    case letT (Proxy @(T "i01" -> l02)) of { LetT (_ :: Proxy l01) ->
    case letT (Proxy @(T "i00" -> l01)) of { LetT (_ :: Proxy l00) ->

      let -- 00 .. 09
          f00 :: f l00
          f01 :: f l01
          f02 :: f l02
          f03 :: f l03
          f04 :: f l04
          f05 :: f l05
          f06 :: f l06
          f07 :: f l07
          f08 :: f l08
          f09 :: f l09
          -- 10 .. 19
          f10 :: f l10
          f11 :: f l11
          f12 :: f l12
          f13 :: f l13
          f14 :: f l14
          f15 :: f l15
          f16 :: f l16
          f17 :: f l17
          f18 :: f l18
          f19 :: f l19
          -- 20 .. 29
          f20 :: f l20
          f21 :: f l21
          f22 :: f l22
          f23 :: f l23
          f24 :: f l24
          f25 :: f l25
          f26 :: f l26
          f27 :: f l27
          f28 :: f l28
          f29 :: f l29

          res :: f r

          -- 00 .. 09
          f00 = pure (castEqual f)
          f01 = castEqual f00 <*> pure (MkT @"i00")
          f02 = castEqual f01 <*> pure (MkT @"i01")
          f03 = castEqual f02 <*> pure (MkT @"i02")
          f04 = castEqual f03 <*> pure (MkT @"i03")
          f05 = castEqual f04 <*> pure (MkT @"i04")
          f06 = castEqual f05 <*> pure (MkT @"i05")
          f07 = castEqual f06 <*> pure (MkT @"i06")
          f08 = castEqual f07 <*> pure (MkT @"i07")
          f09 = castEqual f08 <*> pure (MkT @"i08")
          -- 10 .. 19
          f10 = castEqual f09 <*> pure (MkT @"i09")
          f11 = castEqual f10 <*> pure (MkT @"i10")
          f12 = castEqual f11 <*> pure (MkT @"i11")
          f13 = castEqual f12 <*> pure (MkT @"i12")
          f14 = castEqual f13 <*> pure (MkT @"i13")
          f15 = castEqual f14 <*> pure (MkT @"i14")
          f16 = castEqual f15 <*> pure (MkT @"i15")
          f17 = castEqual f16 <*> pure (MkT @"i16")
          f18 = castEqual f17 <*> pure (MkT @"i17")
          f19 = castEqual f18 <*> pure (MkT @"i18")
          -- 20 .. 29
          f20 = castEqual f19 <*> pure (MkT @"i19")
          f21 = castEqual f20 <*> pure (MkT @"i20")
          f22 = castEqual f21 <*> pure (MkT @"i21")
          f23 = castEqual f22 <*> pure (MkT @"i22")
          f24 = castEqual f23 <*> pure (MkT @"i23")
          f25 = castEqual f24 <*> pure (MkT @"i24")
          f26 = castEqual f25 <*> pure (MkT @"i25")
          f27 = castEqual f26 <*> pure (MkT @"i26")
          f28 = castEqual f27 <*> pure (MkT @"i27")
          f29 = castEqual f28 <*> pure (MkT @"i28")

          res = castEqual f29 <*> pure (MkT @"i29")

      in res

    }}}}}}}}}}
    }}}}}}}}}}
    }}}}}}}}}}
