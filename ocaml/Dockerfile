FROM ocaml/opam2:4.10
WORKDIR /home/opam/src
COPY main.ml .
COPY _tags .
USER root
RUN apt-get install -qy m4
RUN chown -R opam:opam .
USER opam
RUN opam install batteries
RUN eval $(opam config env) && ocamlbuild -use-ocamlfind main.native
ENTRYPOINT ["./main.native"]
