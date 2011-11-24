
EXAMPLES= \
	example_from_python

all: $(EXAMPLES)

example_from_python: example_from_python.ml
	ocamlopt -I .. -o $@ ../ordf.cmxa $<

	