FILES := library.dylan \
	 interface.dylan \
	 transform.dylan \
	 collect.dylan \
	 productions.dylan

libxml-parser.a: $(FILES)
	d2c -L ../meta xml-parser.lid

clean:
	rm *.o *.c *~ cc-*.mak *.a *.du

tarball:
	tar cvf xml.tar $(FILES) Makefile xml-parser.lid; gzip xml.tar

