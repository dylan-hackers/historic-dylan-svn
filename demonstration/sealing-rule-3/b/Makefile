b.lib.du: b.lid b.dylan b-exports.dylan
	d2c -L../a/ b.lid

clean:
	-rm -f *.lo *.o *.s *.la *.a *.c *.mak *~ b.lib.du
	-rm -rf .libs

install: b.lib.du 
	libtool /usr/bin/install -c libb.a b.lib.du `d2c --dylan-user-location`
