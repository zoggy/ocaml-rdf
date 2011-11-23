
C_INCLUDES=-I /usr/include
C_COMPFLAGS=$(C_INCLUDES)

INCLUDES=
COMPFLAGS=$(INCLUDES) -annot
OCAMLPP=

OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt -g
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDOC=ocamldoc
OCAMLDOCOPT=ocamldoc.opt
CAMLP4O=camlp4o
OCAMLLIB:=`$(OCAMLC) -where`

ADDITIONAL_LIBS=
ADDITIONAL_LIBS_BYTE=

INSTALLDIR=$(OCAMLLIB)/rdf

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

SYSLIBS=
SYSLIBS_BYTE=

LIB_C_FILES=

LIB_CMXFILES=

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)

LIB=rdf.cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

all: opt byte

opt: $(LIB)
byte: $(LIB_BYTE)

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LIB_CMXFILES)


$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLC) -a -o $@ $(LIB_CMOFILES)

clean:
	$(RM) *.o *.cm* .annot *.a *.so