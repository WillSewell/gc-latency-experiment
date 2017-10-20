FROM ocaml/opam:debian-stable_ocaml-4.04.2
WORKDIR /home/opam/src
COPY main.ml .
COPY _tags .
USER root
RUN chown -R opam:opam .
USER opam
RUN opam install batteries
RUN eval $(opam config env) && ocamlbuild -use-ocamlfind main.native
ENTRYPOINT ["./main.native"]
