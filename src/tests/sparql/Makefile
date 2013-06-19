
KO='\e[1;31m'"*KO\033[1m\033[0m"
OK='\e[1;32m'"OK\033[1m\033[0m"

all: good bad

good:
	@for i in `ls *.rq | grep -v -E "^bad-"` ; do \
		(../../test_sparql -f $${i} \
		&& /bin/echo -e $(OK) "$$i" ) || /bin/echo -e $(KO) "$$i"; \
	done

bad:
	@for i in bad-*.rq ; do \
		(../../test_sparql -f $${i} \
		&& /bin/echo -e $(KO) "$$i" ) || /bin/echo -e $(OK) "$$i"; \
	done

printer:
	for i in *.rq; do \
		../../test_sparql -f $${i} -p 2> /dev/null > $${i}.1 && \
		 ((../../test_sparql -f $${i}.1 -p > $${i}.2 && /bin/echo -e $(OK) "$${i}") || /bin/echo -e $(KO) "$${i}";) \
	done

clean:
	rm -f *.1 *.2