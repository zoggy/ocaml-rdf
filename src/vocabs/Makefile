
GEN=../tools/gen_ns_mod --ttl

all: acl dc doap foaf earl ldp owl pim prov rdf rdfs sioc solid time vcard

clean:
	rm -f *.ml *.mli

acl: acl.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/ns/auth/acl#" $<

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
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/ns/pim/space#" $^

prov: prov.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/ns/prov#" $^

rdf: rdf.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/1999/02/22-rdf-syntax-ns#" $<

rdfs: rdfs.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/2000/01/rdf-schema#" $<

sioc: sioc.ttl
	$(GEN) -f rdf_$@ $@ "http://rdfs.org/sioc/ns#" $<

solid: solid_terms.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/ns/solid/terms#" $<

time: time.ttl
	$(GEN) -f rdf_$@ $@ "http://www.w3.org/2006/time#" $<

vcard: vcard.ttl
	$(GEN) -f rdf_vcard $@ "http://www.w3.org/2006/vcard/ns#" $<
