default:

csv:
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter HList/Baseline --file-is-number >hlist-baseline.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter HList/LetAs    --file-is-number >hlist-letas.csv

images:
	gnuplot benchmarks.gnuplot