all: dydoc.out

src =   support/*.dylan \
        internal-rep/*.dylan \
        source-files/*.dylan \
        markup-parser/*.dylan \
        markup-translator/*.dylan \
        topic-resolver/*.dylan \
        workflows/*.dylan \
        tool/*.dylan

MONDAY_LIBDIR  = /Users/dvoss/Documents/Projects/Dylan-SVN/libs/monday/lib
UTILITY_LIBDIR = /Users/dvoss/Documents/Projects/Dylan-SVN/libs/utilities

libs = -L$(UTILITY_LIBDIR)/command-line-parser \
       -L$(UTILITY_LIBDIR)/peg-parser \
       -L$(MONDAY_LIBDIR)/language/grammar \
       -L$(MONDAY_LIBDIR)/program-representation/source-location

dydoc.out: dydoc.lid $(src) 
	d2c -g $(libs) dydoc.lid

clean:
	-rm -f *.o *.s *.a *.c *.du *.el *.mak *~
	-rm -rf .libs
