cop: cop.lid cop.dylan cop-exports.dylan
	(cd ../shared; make)
	d2c -s -L../shared cop.lid

run:
	server --gui --robber "gui-robber" --copE "show-cop ./cop"

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ cop
	-rm -rf .libs
