include ../../../master.Makefile

KO='\e[1;31m'"*KO\033[1m\033[0m"
OK='\e[1;32m'"OK\033[1m\033[0m"

TEST=../../rdf_sparql_test
ECHO=/bin/echo -e

all:
	for i in *.test; do \
		($(TEST) $$i > `basename "$$i" .test`.out && $(ECHO) $(OK) "$$i" || $(ECHO) $(KO) "$$i")  ; \
	done