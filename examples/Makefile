
EXAMPLES= \
	example1

all: $(EXAMPLES)

.SUFFIXES: .ml

%: %.ml
	ocamlopt -I .. -o $@ unix.cmxa ../ordf.cmxa $<

	