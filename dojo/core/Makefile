OCAMLBUILD=ocamlbuild -use-menhir -use-ocamlfind
OCAMLFIND=ocamlfind

.PHONY: all clean install

all:
	$(OCAMLBUILD) hackojoCore.otarget

clean:
	$(OCAMLBUILD) -clean
	rm *~

install:
	$(OCAMLFIND) install hackojo-core META hackojoCore.cmxa common/extPervasives.mli