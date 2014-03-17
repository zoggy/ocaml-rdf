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

MKSTOG=mk-stog
MKSTOG_OCAML_SESSION=mk-stog-ocaml-session
STOG=./mystog
STOG_OCAML_SESSION=./mystog-ocaml-session
DEST_DIR=`pwd`/../../ocaml-rdf-pages
BASE_URL_OPTION=
STOG_OPTIONS=--stog-ocaml-session "$(STOG_OCAML_SESSION)  -I ../src" -d $(DEST_DIR) $(BASE_URL_OPTION)
LESSC=lessc
CP=cp -f

build:
	$(MAKE) site

style.css: less/*.less
	$(LESSC) less/style.less > $@

site: style.css
	$(STOG) $(STOG_OPTIONS) .
	$(CP) style.css $(DEST_DIR)/style.css

ocamldoc:
	rm -f refdoc/*html ref-doc/style.css
	(cd ../src && make docstog)

test:
	$(MAKE) BASE_URL_OPTION="--site-url file://$(DEST_DIR)" site

stog:$(STOG) $(STOG_OCAML_SESSION)
.PHONY:$(STOG) $(STOG_OCAML_SESSION)
$(STOG):
	$(MKSTOG) -o $@ -package stog.disqus,stog-writing,rdf -linkall

$(STOG_OCAML_SESSION):
	$(MKSTOG_OCAML_SESSION) -o $@ -package rdf

clean:
	rm -fr .stog/cache



	