multimap.lib.du: multimap.lid multimap.dylan library.dylan
	d2c multimap.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ multimap.lib.du
	-rm -rf .libs

install: multimap.lib.du 
	libtool /opt/sfw/bin/ginstall -c libmultimap.a multimap.lib.du `d2c --dylan-user-location`
