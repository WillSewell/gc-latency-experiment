RESULTS = d/results.txt go/results.txt haskell/results.txt java/results.txt ocaml/results.txt php/results.txt racket/results.txt ruby/results.txt node/results.txt dotnet/results.txt python/results.txt c/results.txt erlang/results.txt crystal/results.txt common-lisp/results-sbcl.txt common-lisp/results-sbcl.txt common-lisp/results-ecl-interp.txt common-lisp/results-ecl-compile.txt common-lisp/results-ccl.txt

.PHONY: all clean

all: $(RESULTS)

clean:
	rm -f $(RESULTS)

d/results.txt: d/Dockerfile d/main.d
	docker build -t gc-d d
	docker run gc-d > $@

go/results.txt: go/Dockerfile go/main.go
	docker build -t gc-go go
	docker run gc-go > $@

haskell/results.txt: haskell/Dockerfile haskell/Main.hs
	docker build -t gc-haskell haskell
	docker run gc-haskell > $@

java/results.txt: java/Dockerfile java/Main.java
	docker build -t gc-java java
	docker run gc-java > $@

ocaml/results.txt: ocaml/Dockerfile ocaml/_tags ocaml/main.ml
	docker build -t gc-ocaml ocaml
	docker run gc-ocaml > $@

php/results.txt: php/Dockerfile php/main.php
	docker build -t gc-php php
	docker run gc-php > $@

racket/results.txt: racket/Dockerfile racket/docker-entrypoint.sh racket/main.rkt
	docker build -t gc-racket racket
	docker run gc-racket > $@

ruby/results.txt: ruby/Dockerfile ruby/main.rb
	docker build -t gc-ruby ruby
	docker run gc-ruby > $@

node/results.txt: node/Dockerfile node/main.js
	docker build -t gc-node node
	docker run gc-node > $@

dotnet/results.txt: dotnet/Dockerfile dotnet/dotnet.csproj dotnet/Program.cs
	docker build -t gc-dotnet dotnet
	docker run gc-dotnet > $@

python/results.txt: python/Dockerfile python/main.py
	docker build -t gc-python python
	docker run gc-python > $@

c/results.txt: c/Dockerfile c/main.c
	docker build -t gc-c c
	docker run gc-c ./main > $@

erlang/results.txt: erlang/Dockerfile erlang/main.erl
	docker build -t gc-erlang erlang
	docker run gc-erlang > $@

crystal/results.txt: crystal/Dockerfile crystal/main.cr
	docker build -t gc-crystal crystal
	docker run gc-crystal ./main > $@

common-lisp/results-sbcl.txt: common-lisp/main.lisp
	docker build -f common-lisp/Dockerfile.sbcl -t gc-sbcl common-lisp
	docker run gc-sbcl > $@

common-lisp/results-ecl-interp.txt: common-lisp/main.lisp
	docker build -f common-lisp/Dockerfile.ecl-interp -t gc-ecl-interp common-lisp
	docker run gc-ecl-interp > $@

common-lisp/results-ecl-compile.txt: common-lisp/main.lisp
	docker build -f common-lisp/Dockerfile.ecl-compile -t gc-ecl-compile common-lisp
	docker run gc-ecl-compile > $@

common-lisp/results-ccl.txt: common-lisp/main.lisp
	docker build -f common-lisp/Dockerfile.ccl -t gc-ccl common-lisp
	docker run gc-ccl > $@
