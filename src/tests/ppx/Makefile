include ../../../master.Makefile

all: test_ppx

test_ppx: test_ppx.ml
	$(OCAMLFIND) ocamlopt -o $@ -package $(PACKAGES) -dsource -thread \
	-ppx ../../ppx_rdf -I ../.. -linkpkg ../../rdf.cmxa $<

clean:
	$(RM) *.cm* test_ppx *.o
