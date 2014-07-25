.PHONY: all clean install uninstall reinstall quietbuild

OCAMLBUILD=ocamlbuild -use-menhir -use-ocamlfind
OCAMLFIND=ocamlfind

MLIS := $(wildcard common/*.mli reactive/*.mli machine/*.mli)
CMIS := $(wildcard _build/common/*.cmi _build/reactive/*.cmi _build/machine/*.cmi)



all:
	$(OCAMLBUILD) hackojoCore.otarget

quietbuild:
	@ echo 'Rebuilding Hackojo CORE...'
	$(OCAMLBUILD) hackojoCore.otarget

clean:
	$(OCAMLBUILD) -clean
	rm --force *~

uninstall:
	@ echo 'Removing exisintg Hackojo CORE installation...'
	$(shell $(OCAMLFIND) remove hackojo-core 2> uninstall.log)

install:
	@ echo 'Installing Hackojo CORE...'
	$(shell ocamlfind install hackojo-core META hackojoCore.cmxa hackojoCore.a $(MLIS) $(CMIS) 2> install.log)

reinstall: quietbuild uninstall install