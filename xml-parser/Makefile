FILES := library \
	 interface \
	 transform \
	 collect \
	 productions

libxml-parser.a: $(FILES:.dylan)
	d2c -L ../meta xml-parser.lid

clean:
	rm *.o *.c *~ cc-*.mak *.a *.du

