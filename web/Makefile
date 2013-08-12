STOG=stog
DEST_DIR=/tmp/ocaml-rdf
BASE_URL_OPTION=
STOG_OPTIONS=-d $(DEST_DIR) $(BASE_URL_OPTION) --package stog.disqus
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
	(cd .. && make doc) && cp -r ../src/ocamldoc/* $(DEST_DIR)/refdoc/

test:
	$(MAKE) BASE_URL_OPTION="--site-url file://$(DEST_DIR)" site

clean:
	rm -fr .stog/cache



	