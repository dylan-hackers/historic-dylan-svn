INSTALLPATH=$(shell d2c --compiler-info | grep D2C_RUNTIME_SUBDIR | sed 's/_DCI_D2C_RUNTIME_SUBDIR=//' | sed s'/ //g')

FILES := library.dylan \
	 interface.dylan \
	 transform.dylan \
	 printing.dylan \
	 collect.dylan \
	 latin1-entities.dylan \
	 productions.dylan

libxml-parser.a: $(FILES)
	d2c -L ../anaphora -L ../meta -L ../../examples/multimap xml-parser.lid

clean:
	rm *.o *.c *~ cc-*.mak *.a *.du

install: xml-parser.lib.du 
	libtool --mode=install /usr/bin/install -c libxml-parser-dylan.la /usr/lib/dylan/$(INSTALLPATH)/libxml-parser-dylan.la
	libtool --finish /usr/lib/dylan/$(INSTALLPATH)/dylan-user
	/usr/bin/install -c xml-parser.lib.du /usr/lib/dylan/$(INSTALLPATH)/xml-parser.lib.du

tarball:
	tar cvf xml.tar $(FILES) Makefile xml-parser.lid; gzip xml.tar
