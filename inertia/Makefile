INSTALLPATH=$(shell d2c --compiler-info | grep _DCI_D2C_DYLAN_USER_DIR | \
              sed 's/_DCI_D2C_DYLAN_USER_DIR=//' | sed 's/\/dylan-user//' | sed s'/ //g')

LIBTOOL=libtool
LIDFILE=inertia.lid

tests/inertia-test: tests/inertia-test-exports.dylan tests/inertia-test-main.dylan inertia.lib.du
	(cd tests; d2c -L.. inertia-test.lid)

inertia.lib.du: inertia.lid inertia-exports.dylan inertia-gl-utils.dylan inertia-geometry.dylan \
                inertia-shapes.dylan inertia-events.dylan inertia-widgets.dylan inertia-main.dylan
	d2c inertia.lid

clean:
	rm -f *.mak *.lib.du *.lo *.o *.la *.a *.c
	(cd tests; rm -f *.mak *.lib.du *.lo *.o *.la *.a *.c inertia-test.exe)

install: inertia.lib.du
	$(LIBTOOL) --mode=install /usr/bin/install -c libinertia-dylan.la $(INSTALLPATH)/libinertia-dylan.la
	$(LIBTOOL) --finish $(INSTALLPATH)
	/usr/bin/install -c inertia.lib.du $(INSTALLPATH)/inertia.lib.du

