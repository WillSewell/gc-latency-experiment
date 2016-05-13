haskell: Main.hs
	ghc -O2 -optc-O3 Main.hs

run-haskell: haskell
	./Main +RTS -s

ocaml: main.ml
	ocamlbuild -use-ocamlfind main.native

run-ocaml: ocaml
	OCAMLRUNPARAM="v=0x400" ./main.native

clean:
	ocamlbuild -clean
	rm -f Main.o Main.hi
