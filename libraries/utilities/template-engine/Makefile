BASE_LIBDIR = ../..

LIBRARIES = \
	$(BASE_LIBDIR)/utilities/peg-parser \
	$(BASE_LIBDIR)/utilities/slot-visitor \
	$(BASE_LIBDIR)/files-and-io/sequence-stream \
	$(BASE_LIBDIR)/utilities/dynamic-binding

libflags = $(addprefix -L,$(LIBRARIES))

template-engine: library-gd.lid *.dylan
	d2c -g $(libflags) library-gd.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *.el *.lib.du *~
	-rm -rf .libs
