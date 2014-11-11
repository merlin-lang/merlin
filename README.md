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

Merlin must currently be built from source. To build from source, first ensure
that you've installed all dependencies, which are listed in the `_oasis` file
under the merlin `Library` sections. To install dependencies that are part of
the [Frenetic project](http://frenetic-lang.org), you may want to install a
custom [OPAM](http://opam.ocamlpro.com/) repository maintained that the Frenetic
project maintains.

    $ opam repository add frenetic https://github.com/frenetic-lang/opam-bleeding.git
    $ opam update

Once that's done, packages that are part of the Frenetic project will be
installed from the HEAD of their master branch on Github.

If you prefer to install dependencies from source, you may build them from their
Github repositories:

  * [DPRLE](https://github.com/frenetic-lang/dprle)
  * [OCaml-Packet](https://github.com/frenetic-lang/ocaml-packet)
  * [OCaml-OpenFlow](https://github.com/frenetic-lang/ocaml-openflow)
  * [OCaml-Topology](https://github.com/frenetic-lang/ocaml-topology)
  * [Frenetic](https://github.com/frenetic-lang/frenetic)

Install those packages and then build and test Merlin using the following
commands:

    $ make
    $ make test


