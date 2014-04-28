PROJECT_NAME := 4stb

FILES :=  rdf_sparql_http.mli rdf_sparql_http.ml rdf_4s.mli rdf_4s.ml main.ml

PACKAGES := lwt.syntax,lwt,cohttp,cohttp.lwt,yojson,rdf

all:
	ocamlfind ocamlc -syntax camlp4o -package $(PACKAGES) -linkpkg -o $(PROJECT_NAME) $(FILES)

clean:
	rm *.cmi *.cmo $(PROJECT_NAME)