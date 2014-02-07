.PHONY: all clean

OCAMLBUILD=ocamlbuild -use-ocamlfind

all:
	$(OCAMLBUILD) hjc.native
	cp hjc.native hjc

clean:
	$(OCAMLBUILD) -clean

