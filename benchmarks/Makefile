default:

csv:
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter Baseline --file-is-number >baseline.csv

images:
	gnuplot benchmarks.gnuplot