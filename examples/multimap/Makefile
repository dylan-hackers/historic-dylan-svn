INSTALLPATH=$(shell d2c --compiler-info | grep D2C_RUNTIME_SUBDIR | sed 's/_DCI_D2C_RUNTIME_SUBDIR=//' | sed s'/ //g')

multimap.lib.du: multimap.lid multimap.dylan library.dylan
	d2c multimap.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ multimap.lib.du
	-rm -rf .libs

install: multimap.lib.du 
	libtool --mode=install /usr/bin/install -c libmultimap-dylan.la /usr/lib/dylan/$(INSTALLPATH)/libmultimap-dylan.la
	libtool --finish /usr/lib/dylan/$(INSTALLPATH)/dylan-user
	/usr/bin/install -c multimap.lib.du /usr/lib/dylan/$(INSTALLPATH)/multimap.lib.du
