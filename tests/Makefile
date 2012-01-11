
EXAMPLES= \
	memory \
	example_from_python

all: $(EXAMPLES)

example_from_python: example_from_python.ml
	ocamlopt -I .. -o $@ ../ocamlrdf.cmxa $<

memory: memory.ml
	ocamlopt -I .. -o $@ ../ocamlrdf.cmxa $<
