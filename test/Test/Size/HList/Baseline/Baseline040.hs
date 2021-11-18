{-# LANGUAGE OverloadedLabels #-}

module Test.Size.HList.Baseline.Baseline040 where

import Test.Size.HList.Setup
import Test.Size.HList.Index.Ix040

hlist :: HList Fields
hlist =
      -- 00 .. 09
      HCons #i00
    $ HCons #i01
    $ HCons #i02
    $ HCons #i03
    $ HCons #i04
    $ HCons #i05
    $ HCons #i06
    $ HCons #i07
    $ HCons #i08
    $ HCons #i09
      -- 10 .. 19
    $ HCons #i10
    $ HCons #i11
    $ HCons #i12
    $ HCons #i13
    $ HCons #i14
    $ HCons #i15
    $ HCons #i16
    $ HCons #i17
    $ HCons #i18
    $ HCons #i19
      -- 20 .. 29
    $ HCons #i20
    $ HCons #i21
    $ HCons #i22
    $ HCons #i23
    $ HCons #i24
    $ HCons #i25
    $ HCons #i26
    $ HCons #i27
    $ HCons #i28
    $ HCons #i29
      -- 30 .. 39
    $ HCons #i30
    $ HCons #i31
    $ HCons #i32
    $ HCons #i33
    $ HCons #i34
    $ HCons #i35
    $ HCons #i36
    $ HCons #i37
    $ HCons #i38
    $ HCons #i39
    $ HNil

