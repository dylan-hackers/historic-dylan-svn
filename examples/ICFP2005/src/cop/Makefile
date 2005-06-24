go: cop
	./cop

cop: cop.lid cop.dylan cop-exports.dylan
	d2c -s -L../shared cop.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ cop
	-rm -rf .libs
