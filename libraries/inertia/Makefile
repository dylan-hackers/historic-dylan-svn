INSTALLPATH=$(shell d2c --compiler-info | grep _DCI_D2C_DYLAN_USER_DIR | \
              sed 's/_DCI_D2C_DYLAN_USER_DIR=//' | sed 's/\/dylan-user//' | sed s'/ //g')

LIBTOOL=libtool
LIDFILE=inertia.lid

tests/inertia-test: tests/inertia-test-exports.dylan tests/inertia-test-main.dylan inertia.lib.du
	(cd tests; d2c -L.. inertia-test.lid)

inertia.lib.du: inertia.lid inertia-exports.dylan inertia-gl-utils.dylan inertia-geometry.dylan \
                inertia-shapes.dylan \
                inertia-shapes/shape-polygon.dylan \
                inertia-shapes/shape-rectangle.dylan \
                inertia-shapes/shape-shape-menu.dylan \
                inertia-events.dylan inertia-effects.dylan \
                inertia-widgets.dylan \
                inertia-widgets/widget-shape-editor.dylan \
                inertia-widgets/widget-button.dylan \
                inertia-widgets/widget-window.dylan \
                inertia-main.dylan
	d2c inertia.lid

test: tests/inertia-test
	(cd tests; ./inertia-test)

clean:
	rm -rf *.mak *.lib.du *.lo *.o *.la *.a *.c .libs
	(cd tests; rm -rf *.mak *.lib.du *.lo *.o *.la *.a *.c inertia-test.exe .libs)

install: inertia.lib.du
	$(LIBTOOL) --mode=install /usr/bin/install -c libinertia-dylan.la $(INSTALLPATH)/libinertia-dylan.la
	$(LIBTOOL) --finish $(INSTALLPATH)
	/usr/bin/install -c inertia.lib.du $(INSTALLPATH)/inertia.lib.du

