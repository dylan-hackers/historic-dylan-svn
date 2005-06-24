go: cop
	./cop

SHARED=../shared/world.dylan

cop: cop.lid cop.dylan cop-exports.dylan ${SHARED}
	d2c -s cop.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ cop
	-rm -rf .libs
