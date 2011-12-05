

OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt -g
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDOC=ocamldoc
OCAMLDOCOPT=ocamldoc.opt
CAMLP4O=camlp4o
OCAMLLIB:=`$(OCAMLC) -where`
OCAMLMKLIB=ocamlmklib

GCC=gcc

C_INCLUDES=-I /usr/include -I /usr/include/rasqal -I /usr/include/raptor2 -I $(OCAMLLIB)/caml
C_COMPFLAGS=$(C_INCLUDES)
LIB_LINKFLAGS= -cclib -lrdf -cclib -lrasqal -cclib -lraptor2
INCLUDES=
COMPFLAGS=$(INCLUDES) -annot
OCAMLPP=

ADDITIONAL_LIBS=
ADDITIONAL_LIBS_BYTE=

INSTALLDIR=$(OCAMLLIB)/rdf

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

SYSLIBS=
SYSLIBS_BYTE=

LIB_C_FILES= \
	wrappers.c \
	ml_types.c \
	ml_raptor.c \
	ml_rasqal.c \
	ml_init.c \
	ml_stream.c \
	ml_parser.c \
	ml_hash.c \
	ml_uri.c \
	ml_node.c \
	ml_statement.c \
	ml_iterator.c \
	ml_query_results.c \
	ml_query.c \
	ml_storage.c \
	ml_model.c


LIB_O_FILES=$(LIB_C_FILES:.c=.o)

LIB_CMXFILES= \
	rdf_pointer.cmx \
	rdf_enums.cmx \
	rdf_types.cmx \
	rdf_misc.cmx \
	rdf_raptor.cmx \
	rdf_uri.cmx \
	rdf_rasqal.cmx \
	rdf_init.cmx \
	rdf_stream.cmx \
	rdf_parser.cmx \
	rdf_hash.cmx \
	rdf_node.cmx \
	rdf_statement.cmx \
	rdf_iterator.cmx \
	rdf_query_results.cmx \
	rdf_query.cmx \
	rdf_storage.cmx \
	rdf_model.cmx

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)

LIB_NAME=ordf
LIB=$(LIB_NAME).cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

all: opt byte

opt: lib
byte: lib_byte

enums: ml_enums.c ml_enums.h rdf_enums.ml

lib: enums $(LIB_O_FILES) $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLMKLIB) -o $(LIB_NAME) $(LIB_O_FILES) $(LIB_CMXFILES) \
	$(LIB_LINKFLAGS)

lib_byte: enums $(LIB_O_FILES) $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLMKLIB) -custom -o $(LIB_NAME) $(LIB_O_FILES) $(LIB_CMOFILES) \
	$(LIB_LINKFLAGS)

example: $(LIB) example.ml
	$(OCAMLOPT) -verbose -o $@ unix.cmxa $^

example.byte: $(LIB_BYTE) example.ml
	$(OCAMLC) -verbose -o $@ unix.cma $^


varcc: varcc.ml4
	$(OCAMLC) -o $@ -pp "$(CAMLP4O) -impl" -impl $<

ml_enums.c ml_enums.h rdf_enums.ml: ml_enums.var varcc
	./varcc $<

############
doc: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLDOC) -load $< -d ocamldoc -html

dump.odoc: rdf*.ml
	$(OCAMLDOC) $(INCLUDES) rdf*.ml -dump $@

############
clean:
	$(RM) *.o *.cm* *.annot *.a *.so
	$(RM) varcc rdf_enums.ml ml_enums.c ml_enums.h

#############
.SUFFIXES: .mli .ml .cmi .cmo .cmx .cmxs .mll .mly .o .c

%.cmi:%.mli
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmi %.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmx %.o:%.ml
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmxs: %.ml
	$(OCAMLOPT) -shared -o $@ $(OCAMLPP) $(COMPFLAGS) $<

%.o: %.c
	$(OCAMLOPT) $(COMPFLAGS) -c $< && $(MV) `basename $@` `dirname $@`

%.ml:%.mll
	$(OCAMLLEX) $<

%.mli %.ml:%.mly
	$(OCAMLYACC) -v $<

%.o: %.c
	$(OCAMLC) $(C_INCLUDES) -c $<

.PHONY: clean depend

.depend depend:
	ocamldep -pp $(CAMLP4O) *.ml > .depend

include .depend