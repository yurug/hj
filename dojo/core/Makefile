OCAMLBUILD=ocamlbuild -use-menhir -use-ocamlfind

.PHONY: all clean install

all:
	$(OCAMLBUILD) hackojoCore.otarget

clean:
	$(OCAMLBUILD) -clean
	rm *~

install:
	$(OCAMLFIND) install hackojo.core