
EXAMPLES= \
	example1 \
	example2 \
	example3

all: $(EXAMPLES)

.SUFFIXES: .ml

%: %.ml
	ocamlopt -I .. -o $@ unix.cmxa ../ordf.cmxa $<

	