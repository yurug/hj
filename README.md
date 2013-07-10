The Hacking Dojo
==

## Description

The Hacking Dojo is an online plateform meant for students to practice
their programming skills under the wise supervision of their teachers.

## Dependencies

* Ocsigen/Eliom >= 3.0 ([website][ocsigen])
* Qemu ([website][qemu])

## Description of the source distribution

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

- `doc`

   A user manual and documents for developpers.

- `vms`

   This is where virtual machines are expected to be installed.
   (This can be changed.)

- `ressources`

   This is where the state of the system will be stored.
   (This can be changed.)

## Basic installation 

This installation assumes that the source distribution has been copied
into a directory `$(ROOT)` that is accessible only to you and to root.

- Install Ocsigen.
   The easiest way is through [OPAM][opam], the ocaml package manager.

- Install Qemu with kvm enabled.
   You should have a package for qemu in your GNU/Linux distribution.

- In `$(ROOT)/vms/default`, download the default virtual
   machine (or create your [own][debianqemu])

```Shell
wget http://www.pps.univ-paris-diderot.fr/~yrg/debian.img
```

- In `$(ROOT)/vms/default`, create a pair of SSH keys:

```Shell
ssh-keygen -f ssh-keys
```

- In `$(ROOT)/vms/default`, boot the virtual machine:

```Shell
qemu -enable-kvm -hda debian.img -m 512 -redir tcp:424242::22
```

- In `$(ROOT)/vms/default`, copy your public key into the `corrector`'s authorized_keys:

```Shell
scp -P 424242 ssh-keys.pub .ssh/authorized_keys
```

The password is `corrector`.

- Change the password of `corrector`.

```Shell
ssh -p 424242 -i ssh-keys corrector@locahost 'passwd'
```

- Shutdown the virtual machine.

```Shell
ssh -p 424242 -i ssh-keys corrector@locahost 'halt'
```

- In `$(ROOT)`, build the system.

```Shell
make
```

- In `$(ROOT)`, initialize a ressource tree.

```Shell
admin/freshdojo
```

- In `$(ROOT)`, initialize a basic teacher.

```Shell
admin/maketeacher login password
```

- In `$(ROOT)`, deploy the system.

```Shell
sudo make deploy
```

- Connect to http://localhost:80


[ocsigen]: http://www.ocsigen.org
[qemu]: http://www.qemu.org
[opam]: http://opam.ocamlpro.com
[debianqemu]: http://wiki.colar.net/creating_a_qemu_image_and_installing_debian_in_it