RESULTS = haskell/results.txt racket/results.txt

.PHONY: all clean

all: $(RESULTS)

clean:
	rm -f $(RESULTS)

haskell/results.txt: haskell/Dockerfile haskell/Main.hs
	docker build -t gc-haskell haskell
	docker run gc-haskell > $@

racket/results.txt: racket/Dockerfile racket/docker-entrypoint.sh racket/main.rkt
	docker build -t gc-racket racket
	docker run gc-racket > $@
