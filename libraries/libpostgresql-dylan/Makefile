INSTALLPATH=$(shell d2c --compiler-info | grep D2C_RUNTIME_SUBDIR | sed 's/_DCI_D2C_RUNTIME_SUBDIR=//' | sed s'/ //g')

postgresql.lib.du: postgresql.lid postgresql.dylan postgresql-exports.dylan postgresql-intr.dylan library.dylan
	d2c postgresql.lid

postgresql-intr.dylan: postgresql-intr.intr
	melange --d2c -I. -I`gcc -print-file-name=include` postgresql-intr.intr postgresql-intr.dylan
#	melange -m postgresql-exports.dylan --d2c -I. -I`gcc -print-file-name=include` postgresql-intr.intr postgresql-intr.dylan

clean:
	-rm -f *.s *.a *.c *.o *.du postgresql-intr.dylan *~ *.mak *.lo *.la
	-rm -rf .libs
	(cd demos/mysql-test && make clean)

test: postgresql.lib.du
	(cd demos/postgresql-test && make test)

install: postgresql.lib.du 
	libtool --mode=install /usr/bin/install -c libpostgresql-dylan.la $(DESTDIR)/usr/lib/dylan/$(INSTALLPATH)/libpostgresql-dylan.la
	libtool --finish $(DESTDIR)/usr/lib/dylan/$(INSTALLPATH)
	/usr/bin/install -c postgresql.lib.du $(DESTDIR)/usr/lib/dylan/$(INSTALLPATH)/postgresql.lib.du
