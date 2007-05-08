a.lib.du: a.lid a.dylan a-exports.dylan
	d2c a.lid

clean:
	-rm -f *.lo *.o *.s *.la *.a *.c *.mak *~ a.lib.du
	-rm -rf .libs

install: a.lib.du 
	libtool /usr/bin/install -c liba.a a.lib.du `d2c --dylan-user-location`
