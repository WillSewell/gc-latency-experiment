# if you want to use analyze-ocaml-instrumented, set this to a source
# checkout of OCaml 4.03 or later
OCAML_SOURCES=~/Prog/ocaml/github-trunk

all:
	@echo "see Makefile for targets"

# compile Haskell program
haskell: Main.hs
	ghc -O2 -optc-O3 Main.hs

clean::
	rm -f Main.o Main.hi

# run Haskell program and report times
run-haskell: haskell
	./Main +RTS -s 2> haskell.log
	cat haskell.log

analyze-haskell:
	@echo "Worst old-generation pause:"
	@cat haskell.log | grep "Gen  1" | sed "s/ /\n/g" | tail -n 1

# compile OCaml program
ocaml: main.ml
	ocamlbuild -use-ocamlfind main.native

clean::
	ocamlbuild -clean

# run OCaml program; only reports the last time
run-ocaml: ocaml
	./main.native

# you need to "raco pkg install gcstats" first
run-racket:
	PLT_INCREMENTAL_GC=1 racket -l gcstats -t main.rkt | tee racket.log

analyze-racket:
	@grep "Max pause time" racket.log

# run Racket program with debug instrumentation
run-racket-instrumented: main.rkt
	PLTSTDERR=debug@GC PLT_INCREMENTAL_GC=1 racket main.rkt 2> racket.log

# collect histogram from debug instrumentation,
# to be used *after* run-racket
analyze-racket-instrumented:
	cat racket.log | grep -v total | cut -d' ' -f7 | sort -n | uniq --count

# these will only work if OCaml has been built with --with-instrumented-runtime
ocaml-instrumented: main.ml
	ocamlbuild -use-ocamlfind -tag "runtime_variant(i)" main.native

run-ocaml-instrumented: ocaml-instrumented
	OCAML_INSTR_FILE="ocaml.log" ./main.native

analyze-ocaml-instrumented:
	$(OCAML_SOURCES)/tools/ocaml-instr-report ocaml.log | grep "dispatch:" -A13

