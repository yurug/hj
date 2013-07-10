.PHONY: all codedoc clean

all: codedoc

IMPLS = $(wildcard ../../../dojo/*.eliom)
INTFS = $(wildcard ../../../dojo/*.eliomi)

HTMLS = $(IMPLS:.eliom=.impl.html) $(INTFS:.eliomi=.intf.html)

CAML2HTML = caml2html -css

codedoc: $(HTMLS)

%.impl.html: %.eliom
	$(CAML2HTML) -o $(notdir $@) $<

%.intf.html: %.eliomi
	$(CAML2HTML) -o $@ $<

clean:
	rm --force $(notdir $(HTMLS)) *~ *.log