SHARED=../shared/Makefile ../shared/world.lid ../shared/world-exports.dylan \
../shared/util.dylan  ../shared/world.dylan

robber: robber.lid robber.dylan robber-exports.dylan ${SHARED}
	(cd ../shared; make)
	d2c -s -L../shared robber.lid

run:
	server --gui --robber "show-robber ./robber-wrapper"

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ robber
	-rm -rf .libs
