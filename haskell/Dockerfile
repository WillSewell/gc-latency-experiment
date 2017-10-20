FROM haskell:8.2
COPY Main.hs .
RUN ghc -O2 -optc-O3 Main.hs
ENTRYPOINT ["./Main"]

# TODO make all output consistent
