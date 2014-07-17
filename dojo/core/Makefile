OCAMLBUILD=ocamlbuild -use-menhir -use-ocamlfind
OCAMLFIND=ocamlfind

MLIS := $(wildcard common/*.mli reactive/*.mli machine/*.mli)
CMIS := $(wildcard _build/common/*.cmi _build/reactive/*.cmi _build/machine/*.cmi)

.PHONY: all clean install uninstall reinstall

all:
	$(OCAMLBUILD) hackojoCore.otarget

quietbuild:
	$(shell $(OCAMLBUILD) hackojoCore.otarget 2>&1 build.log)

clean:
	$(OCAMLBUILD) -clean
	rm --force *~

uninstall:
	$(shell $(OCAMLFIND) remove hackojo-core 2> uninstall.log)

install:
	$(shell $(OCAMLFIND) install hackojo-core META hackojoCore.cmxa hackojoCore.a $(MLIS) $(CMIS) 2> install.log)

reinstall: quietbuild uninstall install