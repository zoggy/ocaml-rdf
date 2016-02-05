
GEN=../tools/gen_ns_mod --ttl

all: dc doap foaf earl ldp owl pim rdf rdfs

clean:
	rm -f *.ml *.mli

dc: dc.ttl
	$(GEN) -f rdf_$@ $@ "http://purl.org/dc/terms/" $<

doap: doap.ttl
	$(GEN) -f rdf_$@ $@ "http://usefulinc.com/ns/doap#" $<

foaf: foaf.ttl
	$(GEN) -f rdf_$@ $@ "http://xmlns.com/foaf/0.1/" $<

earl: earl.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/ns/earl#" $<

ldp: ldp.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/ns/ldp#" $<

owl: owl.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/2002/07/owl#" $<

pim: pim.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/ns/pim/space#" $<

rdf: rdf.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/1999/02/22-rdf-syntax-ns#" $<

rdfs: rdfs.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/2000/01/rdf-schema#" $<

