.PHONY: clean

SKIP_LIST_LIB = -L..

skip-list-test: skip-list-test.lid *.dylan
	d2c -g $(SKIP_LIST_LIB) skip-list-test.lid

clean:
	-rm -f *.o *.s *.a *.c *.du *.el *.mak *~ skip-list-test
	-rm -rf .libs
