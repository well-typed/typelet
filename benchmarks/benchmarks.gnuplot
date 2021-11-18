set datafile separator ','
set terminal png
set xlabel "Number of fields"
set ylabel "Core size (terms + types + coercions)"
set xrange [0:100]

# Baseline

set output "baseline.png"
plot "baseline.csv" using 1:5 with lines title "Baseline"
