OCAMLBUILD=ocamlbuild -use-menhir -use-ocamlfind
OCAMLFIND=ocamlfind

.PHONY: all clean install uninstall reinstall

all:
	$(OCAMLBUILD) hackojoCore.otarget

clean:
	$(OCAMLBUILD) -clean
	rm *~

uninstall:
	$(OCAMLFIND) remove hackojo-core

install:
	$(OCAMLFIND) install hackojo-core META hackojoCore.cmxa hackojoCore.a common/extPervasives.mli _build/common/extPervasives.cmi

reinstall: uninstall install