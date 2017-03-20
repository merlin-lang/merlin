# Merlin

Merlin provides techniques for verifying that both the partitioned program
components and the delegated sub-policies conform to the global network
policy. Overall, Merlin simplifies the task of network administration by
providing high-level abstractions for specifying network policies and scalable
infrastructure for enforcing them.

Merlin is a new network management framework that allows administrators to
express policies in a high-level, declarative language based on regular
expressions. The compiler automatically partitions those policies into
components that can be placed on a variety of devices including switches,
middleboxes, and end hosts. The compiler uses a constraint-solver to determine
the optimal placement strategy using paramaterizable heuristics. Sub-policies
may be further constrained by network tenants, facilitating management of
federated networks.

## Building Merlin

Merlin must currently be built from source. Merlin is written
in [OCaml](https://ocaml.org) and uses
the [OCaml Package Manager (OPAM)][https://opam.ocaml.org/] to install
dependencies. It is also dependent
on [DPRLE](https://github.com/frenetic-lang/dprle)
and [Frenetic](https://github.com/frenetic-lang/frenetic) which must be built
from source.


Dependencies
============

First [install OPAM](https://opam.ocaml.org/doc/Install.html) using the
instructions for your operating system. Then, set up a proper OCaml environment
by running the following commands:

```
opam init
eval `opam config env`
opam switch install 4.03.0
eval `opam config env`
```

In order to compile Merlin, you will need to install the relevant dependencies
via OPAM using the following command. These are the combined dependencies for
Merlin, Frenetic and DPRLE (described below):

```
opam install ocamlfind oasis core fieldslib cmdliner cstruct \
     async_extended async_parallel \
     menhir sexplib sedlex ppx_import \
     ulex ipaddr tcpip base64 cohttp \
     yojson mparser ocamlgraph quickcheck ounit
```

You will also need to install [DPRLE](https://github.com/frenetic-lang/dprle)
and [Frenetic](https://github.com/frenetic-lang/frenetic) from source.

To install DPRLE:

```
git clone https://github.com/frenetic-lang/dprle.git
cd dprle
make
make install
```

To install Frenetic:

```
git clone https://github.com/frenetic-lang/frenetic.git
cd frenetic
make
make install
```

Finally, you will need to install the Gurobi Optimizer:

* http://www.gurobi.com/index

Building
==========

1. Run 'make`
2. Run 'make test'

Example
==========

$ ./Merlin.native -topo examples/min/min.dot examples/min/min.mln -verbose
