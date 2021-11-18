{-# LANGUAGE OverloadedLabels #-}

module Test.Size.HList.Baseline.Baseline020 where

import Test.Size.HList.Setup
import Test.Size.HList.Index.Ix020

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
    $ HNil

