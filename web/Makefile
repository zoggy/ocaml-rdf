MKSTOG=mk-stog
MKSTOG_OCAML_SESSION=mk-stog-ocaml-session
STOG=./mystog
STOG_OCAML_SESSION=./mystog-ocaml-session
DEST_DIR=/tmp/ocaml-rdf
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
	mkdir -p $(DEST_DIR)/refdoc
	#(cd .. && make doc) && cp -r ../src/ocamldoc/* $(DEST_DIR)/refdoc/

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



	