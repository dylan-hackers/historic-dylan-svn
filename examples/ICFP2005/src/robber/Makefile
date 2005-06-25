robber: robber.lid robber.dylan robber-exports.dylan
	(cd ../shared; make)
	d2c -s -L../shared robber.lid

run:
	server --gui --robber "show-robber ./robber"

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ robber
	-rm -rf .libs
