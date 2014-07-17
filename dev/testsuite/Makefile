OCAMLBUILD=ocamlbuild -use-ocamlfind

.PHONY: all configuration build run clean

all: configuration build run

configuration:
	@ $(MAKE) -s -C ../../dojo/core reinstall

build:
	@ $(shell $(OCAMLBUILD) check.native 1> build.log || cat build.log)

run:
	@ ./check.native

clean:
	$(OCAMLBUILD) -clean