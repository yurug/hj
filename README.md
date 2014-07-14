The HackDojo
==

## Description

The Hacking Dojo is an online plateform meant for students to practice
their programming skills under the wise supervision of their teachers.

## Dependencies

* OCaml >= 4.0
* Ocsigen/Eliom >= 3.0 ([website][ocsigen])
* ocamldap >= 2.2

## Description of the source distribution

- `dojo`
  The source tree of the Dojo.

- `dev`
  Documentation and tools for developpers.

- `doc`
  User manual

- `etc`
  Configuration files

- `bin`
  Administration programs

## Installation

This installation assumes that the source distribution has been copied
into a directory `$(ROOT)` that is accessible only to you.

- Install dependencies.
   The easiest way is through [OPAM][opam], the ocaml package manager.

- Customize etc/hackojo.conf.local.

- Initialize your SSL keys from $(ROOT).
```Shell
bin/initialize.sh
```

# Deployment

- Run the deployment script from `$(ROOT)`.
```Shell
bin/deploy.sh
```

[ocsigen]: http://www.ocsigen.org
[opam]: http://opam.ocamlpro.com

