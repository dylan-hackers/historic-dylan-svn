crypto-hashes.lib.du: crypto-hashes.lid crypto-utils.dylan crypto-hashes.dylan crypto-hashes-exports.dylan
	d2c --no-makefile crypto-hashes.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ crypto-hashes.lib.du
	-rm -rf .libs

install: crypto-hashes.lib.du 
	libtool /usr/bin/install -c libcrypto-hashes.a crypto-hashes.lib.du `d2c --dylan-user-location`
