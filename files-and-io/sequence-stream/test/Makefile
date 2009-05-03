.PHONY: clean

SEQUENCE_STREAM_LIB = -L..

tester: tester.lid *.dylan
	d2c -g $(SEQUENCE_STREAM_LIB) tester.lid

clean:
	-rm -f *.o *.s *.a *.c *.du *.el *.mak *~ tester
	-rm -rf .libs
