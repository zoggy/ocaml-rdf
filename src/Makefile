#################################################################################
#                OCaml-RDF                                                      #
#                                                                               #
#    Copyright (C) 2012 Institut National de Recherche en Informatique          #
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

PACKAGES=mysql
OF_FLAGS=-package $(PACKAGES)
INCLUDES=

COMPFLAGS=$(INCLUDES) -annot
LINKFLAGS=$(INCLUDES)

# The executables and libraries to produce
LIB_RDF=rdf.cmxa
LIB_RDF_BYTE=$(LIB_RDF:.cmxa=.cma)

LIB_MYSQL=rdf_mysql.cmxa
LIB_MYSQL_BYTE=$(LIB_MYSQL:.cmxa=.cma)

# Compilation
#############

LIB_RDF_CMOFILES= \
	rdf_config.cmo \
	rdf_utf8.cmo \
	rdf_types.cmo \
	rdf_misc.cmo \
	rdf_graph.cmo

LIB_RDF_CMIFILES= $(LIB_RDF_CMOFILES:.cmo=.cmi)
LIB_RDF_CMXFILES= $(LIB_RDF_CMOFILES:.cmo=.cmx)

LIB_MYSQL_CMOFILES=rdf_my.cmo
LIB_MYSQL_CMIFILES=$(LIB_MYSQL_CMOFILES:.cmo=.cmi)
LIB_MYSQL_CMXFILES=$(LIB_MYSQL_CMOFILES:.cmo=.cmx)

RDF_LIBS=$(LIB_RDF) $(LIB_MYSQL)
RDF_LIBS_BYTE=$(LIB_RDF_BYTE) $(LIB_MYSQL_BYTE)

all: opt byte
opt: $(RDF_LIBS)
byte: $(RDF_LIBS_BYTE)

$(LIB_RDF): $(LIB_RDF_CMIFILES) $(LIB_RDF_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LIB_RDF_CMXFILES)

$(LIB_RDF_BYTE): $(LIB_RDF_CMIFILES) $(LIB_RDF_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LIB_RDF_CMOFILES)

$(LIB_MYSQL): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMXFILES)

$(LIB_MYSQL_BYTE): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMOFILES)

test_mysql: $(RDF_LIBS) rdf_test_mysql.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ $(LIB_RDF) $(LIB_MYSQL) -linkall rdf_test_mysql.cmx

# Documentation :
#################
dump.odoc: *.mli *.ml
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) -package mysql $(OCAMLPP) $(INCLUDES) -sort -dump dump.odoc \
	`ls $^ | grep -v example`

doc: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(OCAMLPP) \
	-load $^ \
	-d ocamldoc -html

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
		$(LIB_RDF) $(LIB_RDF:.cmxa=.a) $(LIB_RDF_CMIFILES) \
		$(LIB_MYSQL) $(LIB_MYSQL:.cmxa=.cmi) $(LIB_MYSQL:.cmxa=.a) $(LIB_MYSQL:.cmxa=.mli)

uninstall: dummy
	$(OCAMLFIND) remove $(PACKAGE)

###########################
# additional dependencies
###########################
