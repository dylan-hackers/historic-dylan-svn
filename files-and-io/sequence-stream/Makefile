.PHONY: sequence-stream clean test

sequence-stream: sequence-stream.lid *.dylan
	d2c -g sequence-stream.lid

clean:
	-rm -f *.o *.s *.a *.c *.du *.el *.mak *~
	-rm -rf .libs

test:
	make -C test clean
	make -C test
	test/tester
