INSTALLPATH=$(shell d2c --compiler-info | grep D2C_RUNTIME_SUBDIR | sed 's/_DCI_D2C_RUNTIME_SUBDIR=//' | sed s'/ //g')

libpng.lib.du: libpng.dylan libpng-intr.dylan libpng.lid libpng-internal-exports.dylan libpng-exports.dylan libpng-test.dylan
	d2c libpng.lid

libpng-intr.dylan: libpng-intr.intr
	melange --d2c -I`gcc --print-file-name=include` -m libpng-internal-exports.dylan, libpng-intr.intr libpng-intr.dylan,
	mv libpng-intr.dylan, libpng-intr.dylan
	mv libpng-internal-exports.dylan, libpng-internal-exports.dylan

clean:
	-rm -f *.mak *.lib.du *.o *.lo *.la *.a *.s *.c ,* *~ \
	libpng-intr.dylan libpng-internal-exports.dylan
	-rm -rf .libs

install: libpng.lib.du 
	libtool --mode=install /usr/bin/install -c liblibpng-dylan.la $(DESTDIR)/usr/lib/dylan/$(INSTALLPATH)/liblibpng-dylan.la
	libtool --finish $(DESTDIR)/usr/lib/dylan/$(INSTALLPATH)/dylan-user
	/usr/bin/install -c libpng.lib.du $(DESTDIR)/usr/lib/dylan/$(INSTALLPATH)/libpng.lib.du
