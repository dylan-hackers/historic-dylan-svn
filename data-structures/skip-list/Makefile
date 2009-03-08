.PHONY: skip-list clean test

skip-list: skip-list.lid *.dylan
	d2c -g skip-list.lid

clean:
	-rm -f *.o *.s *.a *.c *.du *.el *.mak *~
	-rm -rf .libs

test:
	make -C test clean
	make -C test
	test/skip-list-test
