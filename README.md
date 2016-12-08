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

Merlin must currently be built from source. To build from source,
first ensure that you've installed all dependencies, 

Dependencies
============

In order to compile Merlin, you will need to install the following
packages, which are available from `opam`:

* ocaml (>= 4.01) for all, test all_tests
* findlib
* async
* core 
* cstruct
* dprle
* frenetic
* ocamlgraph
* ppx_jane 
* ppx_inline_test

You will also need to install these packages from source:

* [DPRLE](https://github.com/frenetic-lang/dprle)
* [Frenetic](https://github.com/frenetic-lang/frenetic)

Finally, you will need to install the Gurobi Optimizer:

* http://www.gurobi.com/index

Building
==========

1. Run 'make`
2. Run 'make test'

Example
==========

$ ./Merlin.native -topo examples/min/min.dot examples/min/min.mln -verbose
