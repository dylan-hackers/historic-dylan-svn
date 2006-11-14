cgi.lib.du: cgi.lid cgi.dylan cgi-exports.dylan
	d2c cgi.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ cgi.lib.du
	-rm -rf .libs

install: cgi.lib.du 
	libtool /usr/bin/install -c libcgi.a cgi.lib.du `d2c --dylan-user-location`
