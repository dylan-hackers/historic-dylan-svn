test: test.lid test.dylan test-exports.dylan
	d2c -L.. test.lid
	./test

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ test
	-rm -rf .libs
