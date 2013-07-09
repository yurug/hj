The Hacking Dojo
==

## Description

The Hacking Dojo is an online plateform meant for students to practice
their programming skills under the wise supervision of their teachers.

## Dependencies

* Ocsigen/Eliom >= 3.0 ([website][ocsigen])
* Qemu ([website][qemu])

## Description of the distribution

- `dojo` 

   A server that organizes the practice of exercises by students, the
   construction of exercises by teachers and the evaluation of the
   exercises by the teachers (greatly) helped by the machine. It
   provides a web API and a web-based user interface. It also manages
   the state of the system in a git-versioned file system.

- `hj`

   A command-line client for the dojo web API.

- `admin`

   A set of administration tools.

- `etc`

   A basic configuration file.

- `doc'

   A user manual and documents for developpers.

- `vms`

   This is where virtual machines are expected to be installed.
   (This can be changed).

- `ressources`

   This is where the state of the system will be stored.
   (This can be changed).

## Installation 

1. Install Ocsigen.
   The easiest way is through [OPAM][opam], the ocaml package manager.

2. Install Qemu with kvm.
   You should have a package for qemu in your GNU/Linux distribution.

3. Download the default virtual machine.

4. Initialize a ressource tree.


[ocsigen]: http://www.ocsigen.org
[qemu]: http://www.qemu.org
[opam]: http://opam.ocamlpro.com