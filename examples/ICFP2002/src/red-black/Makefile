red-black.lib.du: red-black.lid red-black.dylan red-black-exports.dylan
	d2c red-black.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ red-black.lib.du *.la *.lo
	-rm -rf .libs

install: red-black.lib.du 
	libtool /usr/bin/install -c libred-black.a red-black.lib.du `d2c --dylan-user-location`
