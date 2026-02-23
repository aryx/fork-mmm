# Build mmm with OCaml 4.14.2 via OPAM on Ubuntu, with both regular
# make and dune.
# See also .github/workflows/docker.yml for its use in Github Actions (GHA).

FROM ubuntu:22.04

RUN apt-get update # needed otherwise can't find any package

# Setup a basic C dev environment and Tk
RUN apt-get install -y build-essential autoconf automake pkgconf
# Installing 'tk' below pulls 'tzdata' which is asking questions
# so setting those vars to disable the questions.
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
RUN apt-get install -y tk-dev tcl-dev libx11-dev 
RUN apt-get install -y libssl-dev

# Setup OPAM and OCaml
RUN apt-get install -y opam
RUN opam init --disable-sandboxing -y  # (disable sandboxing due to Docker)
ARG OCAML_VERSION=4.14.2
RUN opam switch create ${OCAML_VERSION} -v

WORKDIR /src

# Install dependencies, copy enough files for configure below to work
COPY configure mmm.opam ./
RUN ./configure

# Now let's build from source
COPY . .

RUN eval $(opam env) && make depend

# ocamltk
RUN eval $(opam env) && cd external/ocamltk && ./configure --with-config=./site.config && make && make opt && make install

# mmm
RUN eval $(opam env) && make
RUN eval $(opam env) && make opt

# Test
#TODO: RUN ./test.sh
RUN ./mmm --help
RUN ./htparse --help

# try with dune
RUN make clean
#ugly
RUN eval $(opam env) && cp -a `ocamlc -where`/camltk `ocamlc -where`/../
RUN eval $(opam env) && cp external/META `ocamlc -where`/../camltk/
# no linking of mmm yet, but at least it compiles all dirs
RUN eval $(opam env) && dune build
