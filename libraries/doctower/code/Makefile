all: doctower

src =	common/*.dylan \
	conditions/*.dylan \
	ordered-tree/*.dylan \
	internal-rep/*.dylan \
	configs/*.dylan \
	dylan-parser/*.dylan \
	dylan-translator/*.dylan \
	source-files/*.dylan \
	markup-parser/*.dylan \
	markup-translator/*.dylan \
	topic-resolver/*.dylan \
	tasks/*.dylan \
	tool/*.dylan \
	parser-common/*.dylan

BASE_LIBDIR = ../..

libs =	-L$(BASE_LIBDIR)/utilities/command-line-parser \
	-L$(BASE_LIBDIR)/utilities/peg-parser \
	-L$(BASE_LIBDIR)/utilities/dynamic-binding \
	-L$(BASE_LIBDIR)/monday/lib/program-representation/source-location \
	-L$(BASE_LIBDIR)/files-and-io/wrapper-streams

doctower: doctower.lid $(src) 
	d2c -g $(libs) doctower.lid

clean:
	-rm -f *.o *.s *.a *.c *.du *.el *.mak doctower *~
	-rm -rf .libs
