#################################################################################
#                OCaml-RDF                                                      #
#                                                                               #
#    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License version        #
#    3 as published by the Free Software Foundation.                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU General Public License for more details.                               #
#                                                                               #
#    You should have received a copy of the GNU General Public License          #
#    along with this program; if not, write to the Free Software                #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include ../master.Makefile

OF_FLAGS=-package $(PACKAGES),$(LWT_PACKAGES)
INCLUDES=

COMPFLAGS=$(INCLUDES) -safe-string -g $(MT_FLAGS) #-annot
   # beware that outputting annotation takes a long time for the sparql parser
LINKFLAGS=$(INCLUDES)

# The executables and libraries to produce
LIB_RDF=rdf.cmxa
LIB_RDF_CMXS=rdf.cmxs
LIB_RDF_BYTE=$(LIB_RDF:.cmxa=.cma)

LIB_MYSQL_BYTE=$(LIB_MYSQL:.cmxa=.cma)
LIB_MYSQL_CMXS=$(LIB_MYSQL:.cmxa=.cmxs)

LIB_POSTGRESQL_BYTE=$(LIB_POSTGRESQL:.cmxa=.cma)
LIB_POSTGRESQL_CMXS=$(LIB_POSTGRESQL:.cmxa=.cmxs)

LIB_LWT_BYTE=$(LIB_LWT:.cmxa=.cma)
LIB_LWT_CMXS=$(LIB_LWT:.cmxa=.cmxs)

PPX=ppx_rdf
PPX_BYTE=$(PPX).byte

# Compilation
#############

LIB_RDF_CMOFILES= \
	rdf_config.cmo \
	rdf_types.cmo \
	rdf_utf8.cmo \
	rdf_misc.cmo \
	rdf_loc.cmo \
	rdf_rdf.cmo \
	rdf_term.cmo \
	rdf_doap.cmo \
	rdf_earl.cmo \
	rdf_foaf.cmo \
	rdf_dc.cmo \
	rdf_dt.cmo \
	rdf_sedlex.cmo \
	rdf_sparql_types.cmo \
	rdf_sparql_ms.cmo \
	rdf_sparql_vis.cmo \
	rdf_sparql_parser.cmo \
	rdf_sparql_lex.cmo \
	rdf_sparql_map.cmo \
	rdf_sparql_print.cmo \
	rdf_sparql_algebra.cmo \
	rdf_bgp.cmo \
	rdf_graph.cmo \
	rdf_rdfs.cmo \
	rdf_ds.cmo \
	rdf_mem.cmo \
	rdf_dot.cmo \
	rdf_xml.cmo \
	rdf_ttl_types.cmo \
	rdf_ttl_parser.cmo \
	rdf_ttl_lex.cmo \
	rdf_ttl.cmo \
	rdf_sparql_expand.cmo \
	rdf_sparql_eval.cmo \
	rdf_sparql.cmo \
	rdf_json.cmo \
	rdf_json_ld.cmo \
	rdf_sparql_protocol.cmo \
	rdf_sparql_http.cmo

LIB_RDF_CMIFILES= $(LIB_RDF_CMOFILES:.cmo=.cmi)
LIB_RDF_CMXFILES= $(LIB_RDF_CMOFILES:.cmo=.cmx)

LIB_MYSQL_CMOFILES=rdf_my.cmo rdf_my1.cmo rdf_my2.cmo
LIB_MYSQL_CMIFILES=$(LIB_MYSQL_CMOFILES:.cmo=.cmi)
LIB_MYSQL_CMXFILES=$(LIB_MYSQL_CMOFILES:.cmo=.cmx)

LIB_POSTGRESQL_CMOFILES=rdf_pg.cmo
LIB_POSTGRESQL_CMIFILES=$(LIB_POSTGRESQL_CMOFILES:.cmo=.cmi)
LIB_POSTGRESQL_CMXFILES=$(LIB_POSTGRESQL_CMOFILES:.cmo=.cmx)


LIB_LWT_CMOFILES=rdf_sparql_http_lwt.cmo rdf_4s_lwt.cmo
LIB_LWT_CMIFILES=$(LIB_LWT_CMOFILES:.cmo=.cmi)
LIB_LWT_CMXFILES=$(LIB_LWT_CMOFILES:.cmo=.cmx)

RDF_LIBS=$(LIB_RDF) $(LIB_RDF_CMXS) $(LIB_MYSQL) $(LIB_MYSQL_CMXS) \
	$(LIB_POSTGRESQL) $(LIB_POSTGRESQL_CMXS) $(LIB_LWT) $(LIB_LWT_CMXS)
RDF_LIBS_BYTE=$(LIB_RDF_BYTE) $(LIB_MYSQL_BYTE) \
	$(LIB_POSTGRESQL_BYTE) $(LIB_LWT_BYTE)

all: opt byte
opt: $(RDF_LIBS) $(PPX)
byte: $(RDF_LIBS_BYTE) $(PPX_BYTE)
tools: $(LIB_RDF)
	cd tools && $(MAKE) all

testtools: \
	test_ttl \
	test_xml \
	rdf_sparql_bench \
	rdf_sparql_test \
	test_mysql \
	test_pgsql \
	test_sparql \
	rdf_turtle_test

.PHONY: tools

$(LIB_RDF): $(LIB_RDF_CMIFILES) $(LIB_RDF_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -linkall -o $@ $(LIB_RDF_CMXFILES)

$(LIB_RDF_CMXS): $(LIB_RDF_CMIFILES) $(LIB_RDF_CMXFILES)
	$(OCAMLFIND) ocamlopt -linkall -shared -o $@ $(LIB_RDF_CMXFILES)

$(LIB_RDF_BYTE): $(LIB_RDF_CMIFILES) $(LIB_RDF_CMOFILES)
	$(OCAMLFIND) ocamlc -a -linkall -o $@ $(LIB_RDF_CMOFILES)

$(PPX): $(LIB_RDF) ppx_rdf.ml
	$(OCAMLFIND) ocamlopt -linkpkg -o $@ -package compiler-libs.common $(OF_FLAGS) $(COMPFLAGS) $^

$(PPX_BYTE): $(LIB_RDF_BYTE) ppx_rdf.ml
	$(OCAMLFIND) ocamlc -linkpkg -o $@ -package compiler-libs.common $(OF_FLAGS) $(COMPFLAGS) $^

# MYSQL

$(LIB_MYSQL): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMXFILES)

$(LIB_MYSQL_CMXS): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMXFILES)

$(LIB_MYSQL_BYTE): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMOFILES)

# POSTGRESQL

$(LIB_POSTGRESQL): $(LIB_POSTGRESQL_CMIFILES) $(LIB_POSTGRESQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(COMPFLAGS) $(LIB_POSTGRESQL_CMXFILES)

$(LIB_POSTGRESQL_CMXS): $(LIB_POSTGRESQL_CMIFILES) $(LIB_POSTGRESQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(COMPFLAGS) $(LIB_POSTGRESQL_CMXFILES)

$(LIB_POSTGRESQL_BYTE): $(LIB_POSTGRESQL_CMIFILES) $(LIB_POSTGRESQL_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(COMPFLAGS) $(LIB_POSTGRESQL_CMOFILES)

# LWT

$(LIB_LWT): $(LIB_LWT_CMIFILES) $(LIB_LWT_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(COMPFLAGS) $(LIB_LWT_CMXFILES)

$(LIB_LWT_CMXS): $(LIB_LWT_CMIFILES) $(LIB_LWT_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(COMPFLAGS) $(LIB_LWT_CMXFILES)

$(LIB_LWT_BYTE): $(LIB_LWT_CMIFILES) $(LIB_LWT_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(COMPFLAGS) $(LIB_LWT_CMOFILES)

# TEST PROGRAMS

test_mysql: $(LIB_RDF) $(LIB_MYSQL) rdf_test_mysql.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) $(LIB_MYSQL) -linkall $(COMPFLAGS) rdf_test_mysql.cmx

test_pgsql: $(LIB_RDF) $(LIB_MYSQL) rdf_test_pgsql.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) $(LIB_POSTGRESQL) -linkall $(COMPFLAGS) rdf_test_pgsql.cmx

test_xml: $(LIB_RDF) test_xml.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) -linkall $(COMPFLAGS) test_xml.cmx

test_ttl: $(LIB_RDF) test_ttl.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) -linkall $(COMPFLAGS) test_ttl.cmx

test_sparql: $(LIB_RDF) test_sparql.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) -linkall $(COMPFLAGS) test_sparql.cmx


test_uncodepoint: rdf_utf8.cmx rdf_sparql_lex.cmx test_uncodepoint.ml
	$(OCAMLFIND) ocamlopt -package ulex -linkpkg -linkall -o $@ $^

rdf_sparql_test: $(LIB_RDF) rdf_sparql_test.cmx rdf_sparql_test_main.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -package config-file -linkpkg -o $@ -linkall $(COMPFLAGS) $^

rdf_sparql_bench: $(LIB_RDF) $(LIB_MYSQL) $(LIB_POSTGRESQL) rdf_sparql_test.cmx rdf_sparql_bench.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -package config-file -linkpkg -o $@ -linkall -inline 10000 $(COMPFLAGS) $^

rdf_turtle_test: $(LIB_RDF) rdf_turtle_test.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -package config-file -linkpkg -o $@ -linkall -inline 10000 $(COMPFLAGS) $^

test_iri: $(LIB_RDF) test_iri.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -package config-file -linkpkg -o $@ -linkall -inline 10000 $(COMPFLAGS) $^

test_sparql_protocol: $(LIB_RDF) $(LIB_LWT) test_sparql_protocol.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(COMPFLAGS) $^

# Documentation :
#################
dump.odoc: *.mli *.ml
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(MT_FLAGS) $(INCLUDES) -sort -dump dump.odoc \
	`ls $^ | grep -v example | grep -v test | grep -v "bench" | grep -v "_parser.ml" | grep -v "lex.ml" | grep -v ppx_ `

doc: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) \
	-t OCaml-RDF \
	-load $^ \
	-d ocamldoc -html -intro index.text
	#$(CP) ../web/style.css ocamldoc/

docg: dump.odoc
	$(MKDIR) ocamldoc
	ocamldoc \
	-t OCaml-RDF \
	-load $^ intro index.text \
	-d ocamldoc -g odoc_depgraph.cma -width 500 -height 600 -dot-options "-Gnodesep=\"0.12\" -Granksep=0.05"
	$(CP) ../web/style.css ocamldoc/

docstog: dump.odoc
	$(MKDIR) ../web/refdoc
	ocamldoc.opt \
	-t "OCaml-RDF reference documentation" \
	-load $^ -intro index.text \
	-d ../web/refdoc -g odoc_stog.cmxs \
	-g odoc_depgraph.cmxs -width 500 -height 600 -dot-options "-Gnodesep=\"0.12\" -Granksep=0.05"

dot: dep.dot
dep.dot: dump.odoc
	$(OCAMLDOC) -load $< -o $@ -dot -dot-reduce
dep.ps:dep.dot
	dot -Tps -o $@ $<

# backup, clean and depend :
############################

distclean: clean

clean:: dummy
	$(RM) *~ \#*\#
	$(RM) *.cm* *.a *.o *.annot
	$(RM) dump.odoc
	$(RM) test_mysql
	$(RM) test_pgsql
	$(RM) $(PPX) $(PPX_BYTE)
	cd tools && $(MAKE) clean

.depend depend:
	$(RM) .depend
	$(OCAMLFIND) ocamldep -package lwt.syntax,sedlex,lwt.ppx *.ml *.mli > .depend

dummy:

include .depend

#################
# Installation
#################
install: all
	$(OCAMLFIND) install $(PACKAGE) META \
		$(LIB_RDF) $(LIB_RDF_CMXS) $(LIB_RDF:.cmxa=.a) $(LIB_RDF_CMIFILES) $(LIB_RDF_CMXFILES) $(LIB_RDF_BYTE) \
		$(LIB_MYSQL) $(LIB_MYSQL_CMXS) $(LIB_MYSQL:.cmxa=.a) $(LIB_MYSQL_BYTE) \
		$(LIB_POSTGRESQL) $(LIB_POSTGRESQL_CMXS) $(LIB_POSTGRESQL:.cmxa=.a) $(LIB_POSTGRESQL_BYTE) \
		`if test -n "$(LIB_LWT)"; then echo $(LIB_LWT) $(LIB_LWT_CMXS) $(LIB_LWT:.cmxa=.a) $(LIB_LWT_CMIFILES) $(LIB_LWT_CMXFILES) $(LIB_LWT_BYTE) ; fi` \
		$(PPX) $(PPX_BYTE)

uninstall: dummy
	$(OCAMLFIND) remove $(PACKAGE)

###########################
# additional dependencies
###########################

#rdf_ttl_lex.cmx: rdf_ttl_lex.ml
#	$(OCAMLFIND) ocamlopt $(OF_FLAGS) $(OCAMLPP) $(COMPFLAGS) -syntax camlp4o -c $<

rdf_ttl_parser.ml rdf_ttl_parser.mli: rdf_ttl_parser.mly
	$(MENHIR) --explain --base rdf_ttl_parser $^

rdf_sparql_parser.ml rdf_sparql_parser.mli: rdf_sparql_parser.mly
	$(MENHIR) --explain --infer --base rdf_sparql_parser $^  #--trace

rdf_sparql_test.cmx: rdf_sparql_test.ml
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) $(COMPFLAGS) -c -package config-file $<


