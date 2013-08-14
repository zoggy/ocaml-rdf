#################################################################################
#                OCaml-RDF                                                      #
#                                                                               #
#    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     #
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

OF_FLAGS=-package $(PACKAGES)
INCLUDES=

COMPFLAGS=$(INCLUDES) -annot -g $(MT_FLAGS)
LINKFLAGS=$(INCLUDES)

# The executables and libraries to produce
LIB_RDF=rdf.cmxa
LIB_RDF_CMXS=rdf.cmxs
LIB_RDF_BYTE=$(LIB_RDF:.cmxa=.cma)

LIB_MYSQL_BYTE=$(LIB_MYSQL:.cmxa=.cma)
LIB_MYSQL_CMXS=$(LIB_MYSQL:.cmxa=.cmxs)

LIB_POSTGRESQL_BYTE=$(LIB_POSTGRESQL:.cmxa=.cma)
LIB_POSTGRESQL_CMXS=$(LIB_POSTGRESQL:.cmxa=.cmxs)

# Compilation
#############

LIB_RDF_CMOFILES= \
	rdf_config.cmo \
	rdf_utf8.cmo \
	rdf_misc.cmo \
	rdf_loc.cmo \
	rdf_uri.cmo \
	rdf_rdf.cmo \
	rdf_node.cmo \
	rdf_graph.cmo \
	rdf_ds.cmo \
	rdf_mem.cmo \
	rdf_dot.cmo \
	rdf_xml.cmo \
	rdf_ulex.cmo \
	rdf_ttl_types.cmo \
	rdf_ttl_parser.cmo \
	rdf_ttl_lex.cmo \
	rdf_ttl.cmo \
	rdf_dt.cmo \
	rdf_sparql_types.cmo \
	rdf_sparql_vis.cmo \
	rdf_sparql_parser.cmo \
	rdf_sparql_lex.cmo \
	rdf_sparql_map.cmo \
	rdf_sparql_print.cmo \
	rdf_sparql_algebra.cmo \
	rdf_sparql_ms.cmo \
	rdf_sparql_expand.cmo \
	rdf_sparql_eval.cmo \
	rdf_sparql.cmo

LIB_RDF_CMIFILES= $(LIB_RDF_CMOFILES:.cmo=.cmi)
LIB_RDF_CMXFILES= $(LIB_RDF_CMOFILES:.cmo=.cmx)

LIB_MYSQL_CMOFILES=rdf_my.cmo
LIB_MYSQL_CMIFILES=$(LIB_MYSQL_CMOFILES:.cmo=.cmi)
LIB_MYSQL_CMXFILES=$(LIB_MYSQL_CMOFILES:.cmo=.cmx)

LIB_POSTGRESQL_CMOFILES=rdf_pg.cmo
LIB_POSTGRESQL_CMIFILES=$(LIB_POSTGRESQL_CMOFILES:.cmo=.cmi)
LIB_POSTGRESQL_CMXFILES=$(LIB_POSTGRESQL_CMOFILES:.cmo=.cmx)

RDF_LIBS=$(LIB_RDF) $(LIB_RDF_CMXS) $(LIB_MYSQL) $(LIB_MYSQL_CMXS) \
	$(LIB_POSTGRESQL) $(LIB_POSTGRESQL_CMXS)
RDF_LIBS_BYTE=$(LIB_RDF_BYTE) $(LIB_MYSQL_BYTE) $(LIB_POSTGRESQL_BYTE)

all: opt byte
opt: $(RDF_LIBS)
byte: $(RDF_LIBS_BYTE)
tools: $(LIB_RDF)
	cd tools && $(MAKE) all

.PHONY: tools

$(LIB_RDF): $(LIB_RDF_CMIFILES) $(LIB_RDF_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -linkall -o $@ $(LIB_RDF_CMXFILES)

$(LIB_RDF_CMXS): $(LIB_RDF_CMIFILES) $(LIB_RDF_CMXFILES)
	$(OCAMLFIND) ocamlopt -linkall -shared -o $@ $(LIB_RDF_CMXFILES)

$(LIB_RDF_BYTE): $(LIB_RDF_CMIFILES) $(LIB_RDF_CMOFILES)
	$(OCAMLFIND) ocamlc -a -linkall -o $@ $(LIB_RDF_CMOFILES)

$(LIB_MYSQL): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMXFILES)

$(LIB_MYSQL_CMXS): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMXFILES)

$(LIB_MYSQL_BYTE): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMOFILES)

$(LIB_POSTGRESQL): $(LIB_POSTGRESQL_CMIFILES) $(LIB_POSTGRESQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(COMPFLAGS) $(LIB_POSTGRESQL_CMXFILES)

$(LIB_POSTGRESQL_CMXS): $(LIB_POSTGRESQL_CMIFILES) $(LIB_POSTGRESQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(COMPFLAGS) $(LIB_POSTGRESQL_CMXFILES)

$(LIB_POSTGRESQL_BYTE): $(LIB_POSTGRESQL_CMIFILES) $(LIB_POSTGRESQL_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(COMPFLAGS) $(LIB_POSTGRESQL_CMOFILES)

test_mysql: $(RDF_LIBS) rdf_test_mysql.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) $(LIB_MYSQL) -linkall $(COMPFLAGS) rdf_test_mysql.cmx

test_xml: $(LIB_RDF) test_xml.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) -linkall $(COMPFLAGS) test_xml.cmx

test_ttl: $(LIB_RDF) test_ttl.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) -linkall $(COMPFLAGS) test_ttl.cmx

test_sparql: $(LIB_RDF) test_sparql.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) -linkall $(COMPFLAGS) test_sparql.cmx


test_uncodepoint: rdf_utf8.cmx rdf_sparql_lex.cmx test_uncodepoint.ml
	$(OCAMLFIND) ocamlopt -package ulex -linkpkg -linkall -o $@ $^

rdf_sparql_test: $(LIB_RDF) rdf_sparql_test.cmx
		$(OCAMLFIND) ocamlopt $(OF_FLAGS) -package config-file -linkpkg -o $@ $(LIB_RDF) -linkall $(COMPFLAGS) rdf_sparql_test.cmx

# Documentation :
#################
dump.odoc: *.mli *.ml
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(OCAMLPP) $(INCLUDES) -sort -dump dump.odoc \
	`ls $^ | grep -v example | grep -v test | grep -v _parser.ml | grep -v _lex.ml`

doc: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(OCAMLPP) \
	-t OCaml-RDF \
	-load $^ \
	-d ocamldoc -html -intro index.text
	#$(CP) ../web/style.css ocamldoc/

docg: dump.odoc
	$(MKDIR) ocamldoc
	ocamldoc $(OCAMLPP) \
	-t OCaml-RDF \
	-load $^ intro index.text \
	-d ocamldoc -g odoc_depgraph.cma -width 500 -height 600 -dot-options "-Gnodesep=\"0.12\" -Granksep=0.05"
	$(CP) ../web/style.css ocamldoc/

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
	cd tools && $(MAKE) clean

.depend depend:
	$(RM) .depend
	$(OCAMLDEP) *.ml *.mli > .depend

dummy:

include .depend

#################
# Installation
#################
install: all
	$(OCAMLFIND) install $(PACKAGE) META \
		$(LIB_RDF) $(LIB_RDF_CMXS) $(LIB_RDF:.cmxa=.a) $(LIB_RDF_CMIFILES) $(LIB_RDF_CMXFILES) $(LIB_RDF_BYTE) \
		$(LIB_MYSQL) $(LIB_MYSQL_CMXS) $(LIB_MYSQL:.cmxa=.a) $(LIB_MYSQL_BYTE) \
		$(LIB_POSTGRESQL) $(LIB_POSTGRESQL_CMXS) $(LIB_POSTGRESQL:.cmxa=.a) $(LIB_POSTGRESQL_BYTE)

uninstall: dummy
	$(OCAMLFIND) remove $(PACKAGE)

###########################
# additional dependencies
###########################

rdf_ttl_lex.ml: rdf_ttl_lex.mll
	camlp4o -printer Camlp4OCamlPrinter.cmo \
	`$(OCAMLFIND) query ulex`/pa_ulex.cma -impl $^ > $@

#rdf_ttl_lex.cmx: rdf_ttl_lex.ml
#	$(OCAMLFIND) ocamlopt $(OF_FLAGS) $(OCAMLPP) $(COMPFLAGS) -syntax camlp4o -c $<

rdf_ttl_parser.ml rdf_ttl_parser.mli: rdf_ttl_parser.mly
	$(MENHIR) --explain --base rdf_ttl_parser $^

rdf_sparql_lex.ml: rdf_sparql_lex.mll
	camlp4o -printer Camlp4OCamlPrinter.cmo \
	`$(OCAMLFIND) query ulex`/pa_ulex.cma -impl $^ > $@

rdf_sparql_parser.ml rdf_sparql_parser.mli: rdf_sparql_parser.mly
	$(MENHIR) --explain --infer --base rdf_sparql_parser $^ # --trace

rdf_sparql_test.cmx: rdf_sparql_test.ml
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) $(COMPFLAGS) -c -package config-file $<
