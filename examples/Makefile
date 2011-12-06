
EXAMPLES= \
	example1 \
	example2

all: $(EXAMPLES)

.SUFFIXES: .ml

%: %.ml
	ocamlopt -I .. -o $@ unix.cmxa ../ordf.cmxa $<

	