FROM haskell:8.8
COPY Main.hs .
RUN ghc -O2 -optc-O3 Main.hs
ENTRYPOINT ["./Main"]

# TODO make all output consistent
