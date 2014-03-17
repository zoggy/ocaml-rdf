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


include ../../master.Makefile

TOOLS=foaf rdf_to_ttl rdf_to_svgs gen_ns_mod rdfs_to_dot
all: $(TOOLS)

foaf: foaf.ml
	$(OCAMLFIND) ocamlopt -package rdf -linkpkg -linkall -o $@ $^

gen_ns_mod: $(LIB_RDF) gen_ns_mod.ml
	$(OCAMLFIND) ocamlopt -package rdf -linkpkg -linkall -o $@ $^

rdf_to_ttl: $(LIB_RDF) rdf_to_ttl.ml
	$(OCAMLFIND) ocamlopt -package rdf -linkpkg -linkall -o $@ $^

rdf_to_svgs: rdf_to_svgs.ml
	$(OCAMLFIND) ocamlopt -package rdf -linkpkg -linkall -o $@ $^

rdfs_to_dot: rdfs_to_dot.ml
	$(OCAMLFIND) ocamlopt -package rdf -linkpkg -linkall -o $@ $^


clean:
	$(RM) *~ \#*\#
	$(RM) *.cm* *.a *.o *.annot
	$(RM) $(TOOLS)

.PHONY: clean

