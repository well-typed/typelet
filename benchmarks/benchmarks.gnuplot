set datafile separator ','
set terminal png
set xlabel "Number of fields"
set ylabel "Core size (terms + types + coercions)"
set xrange [0:100]

# HList

set output "hlist.png"
plot "hlist-baseline.csv"  using 1:5 with lines title "Baseline"    \
   , "hlist-letas.csv"     using 1:5 with lines title "LetAs"       \
   , "hlist-letas-cps.csv" using 1:5 with lines title "LetAs (CPS)"

set output "simpl-hlist.png"
plot "simpl-hlist-baseline.csv"  using 1:5 with lines title "Baseline"    \
   , "simpl-hlist-letas.csv"     using 1:5 with lines title "LetAs"       \
   , "simpl-hlist-letas-cps.csv" using 1:5 with lines title "LetAs (CPS)"

# HList

set output "ap.png"
plot "ap-baseline.csv" using 1:5 with lines title "Baseline" \
   , "ap-let.csv"      using 1:5 with lines title "Let"

set output "simpl-ap.png"
plot "simpl-ap-baseline.csv" using 1:5 with lines title "Baseline" \
   , "simpl-ap-let.csv"      using 1:5 with lines title "Let"
