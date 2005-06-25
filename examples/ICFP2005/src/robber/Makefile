robber: robber.lid robber.dylan robber-exports.dylan ../shared/libworld-dylan.a
	(cd ../shared; make)
	d2c -s -L../shared robber.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ robber
	-rm -rf .libs
