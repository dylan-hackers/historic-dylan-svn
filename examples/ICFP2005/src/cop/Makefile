cop: cop.lid cop.dylan cop-exports.dylan  ../shared/libworld-dylan.a
	(cd ../shared; make)
	d2c -s -L../shared cop.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ cop
	-rm -rf .libs
