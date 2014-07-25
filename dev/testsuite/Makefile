OCAMLBUILD=ocamlbuild -use-ocamlfind

.PHONY: all configuration build run clean

all: configuration build run

configuration:
	@ $(MAKE) -s -C ../../dojo/core reinstall

build: clean
	@ $(OCAMLBUILD) check.native

run:
	@ ./check.native

clean:
	$(OCAMLBUILD) -clean