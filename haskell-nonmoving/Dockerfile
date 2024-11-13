FROM haskell:9.2
COPY Main.hs .
RUN ghc -threaded -O2 -optc-O3 Main.hs
ENTRYPOINT ["./Main", "+RTS", "--nonmoving-gc"]

# TODO make all output consistent
