.PHONY: clean

WRAPPER_STREAMS_LIB = -L..
SEQUENCE_STREAM_LIB = -L../../sequence-stream

wrapper-streams-tester: tester.lid *.dylan
	d2c -g $(WRAPPER_STREAMS_LIB) $(SEQUENCE_STREAM_LIB) tester.lid

clean:
	-rm -f *.o *.s *.a *.c *.du *.el *.mak *~ wrapper-streams-tester
	-rm -rf .libs
