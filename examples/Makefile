
EXAMPLES= \
	example1 \
	example2 \
	example3 \
	example5 \
	example6

all: $(EXAMPLES)

run:
	for i in $(EXAMPLES); do ./run.sh $$i; done

.SUFFIXES: .ml

%: %.ml
	ocamlopt -g -I .. -o $@ unix.cmxa ../ocamlrdf.cmxa $<

clean:
	rm -f *.cm* *.o *.out $(EXAMPLES) test*.db


	