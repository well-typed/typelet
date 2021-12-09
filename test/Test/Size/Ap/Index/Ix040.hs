module Test.Size.Ap.Index.Ix040 (F) where

import Test.Infra

type F r =
       -- 00 .. 09
       T "i00"
    -> T "i01"
    -> T "i02"
    -> T "i03"
    -> T "i04"
    -> T "i05"
    -> T "i06"
    -> T "i07"
    -> T "i08"
    -> T "i09"
       -- 10 .. 19
    -> T "i10"
    -> T "i11"
    -> T "i12"
    -> T "i13"
    -> T "i14"
    -> T "i15"
    -> T "i16"
    -> T "i17"
    -> T "i18"
    -> T "i19"
       -- 20 .. 29
    -> T "i20"
    -> T "i21"
    -> T "i22"
    -> T "i23"
    -> T "i24"
    -> T "i25"
    -> T "i26"
    -> T "i27"
    -> T "i28"
    -> T "i29"
       -- 30 .. 39
    -> T "i30"
    -> T "i31"
    -> T "i32"
    -> T "i33"
    -> T "i34"
    -> T "i35"
    -> T "i36"
    -> T "i37"
    -> T "i38"
    -> T "i39"
    -> r
