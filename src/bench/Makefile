BENCH=../rdf_sparql_bench

OP_MYSQL='[("storage","mysql");("engine","InnoDB");("database","rdfbench");("user","rdf")]' --id 'mysql(innodb)'
OP_MYSQL_ISAM='[("storage","mysql");("engine","MyISAM");("database","rdfbenchisam");("user","rdf")]' --id 'mysql(isam)'
OP_MYSQL2='[("storage","mysql2");("engine","InnoDB");("database","rdfbench2");("user","rdf")]' --id 'mysql2(innodb)'
OP_MYSQL2_ISAM='[("storage","mysql2");("engine","MyISAM");("database","rdfbench2isam");("user","rdf")]' --id 'mysql2(isam)'
OP_MEM='[("storage","mem")]' --id 'mem'

BACKENDS=OP_MYSQL OP_MYSQL_ISAM OP_MYSQL2 OP_MYSQL2_ISAM OP_MEM

all: import sparql

report:
	$(BENCH) --html ../../web/benchmarks.html

QUERY=1
SIZE=1000

COM_SPARQL=$(BENCH) q$(QUERY)_$(SIZE).test --goptions
COM_IMPORT=$(BENCH) --import q$(QUERY)_$(SIZE).test --goptions

import:
	$(COM_IMPORT) $(OP_MEM)
	$(COM_IMPORT) $(OP_MYSQL)
	$(COM_IMPORT) $(OP_MYSQL_ISAM)
	$(COM_IMPORT) $(OP_MYSQL2)
	$(COM_IMPORT) $(OP_MYSQL2_ISAM)

sparql:
	$(COM_SPARQL) $(OP_MEM)
	$(COM_SPARQL) $(OP_MYSQL)
	$(COM_SPARQL) $(OP_MYSQL_ISAM)
	$(COM_SPARQL) $(OP_MYSQL2)
	$(COM_SPARQL) $(OP_MYSQL2_ISAM)


SIZES=1000 10000 20000 50000 100000 500000 1000000

allimport:
	for i in $(SIZES); do \
		$(MAKE) SIZE=$$i import; \
	done

allsparql:
	for i in $(SIZES); do \
		$(MAKE) SIZE=$$i sparql; \
	done
