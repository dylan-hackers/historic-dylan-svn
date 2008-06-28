.PHONY: wrapper-streams clean test

wrapper-streams: wrapper-streams.lid *.dylan
	d2c -g wrapper-streams.lid

clean:
	-rm -f *.o *.s *.a *.c *.du *.el *.mak *~
	-rm -rf .libs

test:
	make -C test
	test/wrapper-streams-tester
