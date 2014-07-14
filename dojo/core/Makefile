OCAMLBUILD=ocamlbuild -use-menhir -use-ocamlfind

.PHONY: all clean install

all:
	$(OCAMLBUILD) hjcore.otarget

clean:
	$(OCAMLBUILD) -clean
	rm *~