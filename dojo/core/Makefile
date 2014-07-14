OCAMLBUILD=ocamlbuild -use-menhir -use-ocamlfind
OCAMLFIND=ocamlfind

MLIS := $(wildcard common/*.mli reactive/*.mli machine/*.mli)
CMIS := $(wildcard _build/common/*.cmi _build/reactive/*.cmi _build/machine/*.cmi)

.PHONY: all clean install uninstall reinstall

all:
	$(OCAMLBUILD) hackojoCore.otarget

clean:
	$(OCAMLBUILD) -clean
	rm *~

uninstall:
	$(OCAMLFIND) remove hackojo-core

install:
	$(OCAMLFIND) install hackojo-core META hackojoCore.cmxa hackojoCore.a $(MLIS) $(CMIS)

reinstall: uninstall install