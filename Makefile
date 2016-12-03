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

java: Main.java
	javac Main.java

clean::
	rm -f Main.class

run-java: java
	java -verbosegc -cp . Main | tee java.log

analyze-java:
	@echo "longest Java pause time:"
	@cat java.log | grep -o "[0-9.]* secs" | sort -n | tail -n 1

run-java-g1: java
	java -XX:+UseG1GC -verbosegc -cp . Main | tee java-g1.log

analyze-java-g1:
	@echo "longest Java pause time (G1 collector):"
	@cat java-g1.log | grep -v concurrent | grep -o "[0-9.]* secs" | sort -n | tail -n 1
# "concurrent" GC phases run concurrently with the mutator threads,
# which means that the program keeps running, it would be wrong to count
# them as pauses/latencies. (Instead of rejecting "concurrent-*" phases, one
# option is to filter on "pause" events only, but there are events that are
# not concurrents yet not marked as pauses in G1 logs, such as "remark").

# compile Go program
go: main.go
	go build main.go

clean::
	rm -f main

# run Go program and report times
run-go: go
	GODEBUG=gctrace=1 ./main 2> go.log
	cat go.log

analyze-go:
	@echo "Worst pause:"
	@cat go.log | sed -n 's/.*: \([0-9][0-9]*\.*[0-9][0-9]*\)+.*+\([0-9][0-9]*\.*[0-9][0-9]*\).*/\1:\2/p' | tr ':' "\n" | sort | tail -1 | sed 's/$$/ms/'

# compile D program
d-dmd: main.d
	dmd main.d

clean::
	rm -f main main.o

# run D program and report times
run-d-dmd: d-dmd
	./main "--DRT-gcopt=profile:1" > d-dmd.log
	cat d-dmd.log

analyze-d-dmd:
	@grep "Max Pause" d-dmd.log

d-ldc: main.d
	ldc2 main.d

clean::
	rm -f main main.o

run-d-ldc: d-ldc
	./main "--DRT-gcopt=profile:1" > d-ldc.log
	cat d-ldc.log

analyze-d-ldc:
	@grep "Max Pause" d-ldc.log
