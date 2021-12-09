default:

csv:
	# ds
	
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter 'HList/Baseline/Baseline' --file-is-number >hlist-baseline.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter 'HList/LetAs/LetAs'       --file-is-number >hlist-letas.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter 'HList/LetAsCPS/LetAsCPS' --file-is-number >hlist-letas-cps.csv

	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter 'Ap/Baseline/Baseline' --file-is-number >ap-baseline.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter 'Ap/Let/Let'           --file-is-number >ap-let.csv

        # simpl

	ghc-dump summarize `find ../.. -name '*-0001.cbor'` | parse-ghc-dump-summary --sort --filter 'HList/Baseline/Baseline' --file-is-number >simpl-hlist-baseline.csv
	ghc-dump summarize `find ../.. -name '*-0001.cbor'` | parse-ghc-dump-summary --sort --filter 'HList/LetAs/LetAs'       --file-is-number >simpl-hlist-letas.csv
	ghc-dump summarize `find ../.. -name '*-0001.cbor'` | parse-ghc-dump-summary --sort --filter 'HList/LetAsCPS/LetAsCPS' --file-is-number >simpl-hlist-letas-cps.csv

	ghc-dump summarize `find ../.. -name '*-0001.cbor'` | parse-ghc-dump-summary --sort --filter 'Ap/Baseline/Baseline' --file-is-number >simpl-ap-baseline.csv
	ghc-dump summarize `find ../.. -name '*-0001.cbor'` | parse-ghc-dump-summary --sort --filter 'Ap/Let/Let'           --file-is-number >simpl-ap-let.csv
images:
	gnuplot benchmarks.gnuplot
