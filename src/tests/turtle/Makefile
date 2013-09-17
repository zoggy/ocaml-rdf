BASE=http://www.w3.org/2001/sw/DataAccess/df1/tests/

KO='\e[1;31m'"*KO\033[1m\033[0m"
OK='\e[1;32m'"OK\033[1m\033[0m"

all: good bad

good:
	@(../../test_ttl -b $(BASE) test.nt > test.mine && /bin/echo -e $(OK) test.nt ) || /bin/echo -e $(KO) test.nt;
	@for i in base1 base2 escapedNamespace1 prefix1 type; do \
		(../../test_ttl -b $(BASE) $${i}.ttl > $${i}.mine && \
		grep -v "_:" $${i}.out | sort > $${i}.out-sorted && \
		grep -v "_:" $${i}.mine | sort > $${i}.mine-sorted && \
		diff $${i}.out-sorted $${i}.mine-sorted \
		&& /bin/echo -e $(OK) "$$i" ) || /bin/echo -e $(KO) "$$i"; \
	done
	@for i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 30; do \
		(../../test_ttl -b $(BASE) test-$${i}.ttl > test-$${i}.mine && \
		grep -v "_:" test-$${i}.out | sort > test-$${i}.out-sorted && \
		grep -v "_:" test-$${i}.mine | sort > test-$${i}.mine-sorted && \
		diff test-$${i}.out-sorted test-$${i}.mine-sorted \
		&& /bin/echo -e $(OK) "$$i" ) || /bin/echo -e $(KO) "$$i"; \
	done

bad:
	@for i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14; do \
		(../../test_ttl -b $(BASE) bad-$${i}.ttl && /bin/echo -e $(KO) "$$i" ) || /bin/echo -e $(OK) "$$i"; \
	done


clean:
	rm -f *.mine *-sorted
