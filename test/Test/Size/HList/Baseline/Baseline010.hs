{-# LANGUAGE OverloadedLabels #-}

module Test.Size.HList.Baseline.Baseline010 where

import Test.Size.HList.Setup
import Test.Size.HList.Index.Ix010

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
    $ HNil

