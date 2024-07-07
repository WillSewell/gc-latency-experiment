FROM ocaml/opam:debian-ocaml-5.2
WORKDIR /home/opam/src
COPY main.ml .
USER root
RUN chown -R opam:opam .
USER opam
RUN eval $(opam config env) && ocamlopt -I +unix unix.cmxa main.ml -o main.native
ENTRYPOINT ["./main.native"]
