.PHONY: test clean promote

test:
	dune runtest

promote:
	../_build/default/src/exe-spectec/main.exe \
	  ../../specification/wasm-2.0/*.spectec --effects \
	  > expected/effects.csv
	../_build/default/src/exe-spectec/main.exe \
	  ../../specification/wasm-2.0/*.spectec --atom-sem \
	  > expected/atom_sem.json
	../_build/default/src/exe-spectec/main.exe \
	  ../../specification/wasm-3.0/*.spectec --effects \
	  > expected/effects-wasm-3.0.csv

clean:
	rm -f effects.act atom_sem.act
