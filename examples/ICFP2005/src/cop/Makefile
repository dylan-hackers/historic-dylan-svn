SHARED=../shared/Makefile ../shared/world.lid ../shared/world-exports.dylan \
../shared/util.dylan  ../shared/world.dylan

cop: cop.lid cop.dylan cop-exports.dylan ${SHARED}
	(cd ../shared; make)
	d2c -s -L../shared cop.lid

run:
	server --gui --robber "show-robber ../robber/robber-wrapper" --copE "show-cop ./cop"

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ cop
	-rm -rf .libs
