
EXAMPLES= \
	example1 \
	example2 \
	example3

all: $(EXAMPLES)

.SUFFIXES: .ml

%: %.ml
	ocamlopt -I .. -o $@ unix.cmxa ../ocamlrdf.cmxa $<

clean:
	rm -f *.cm* *.o $(EXAMPLES) test*.db


	