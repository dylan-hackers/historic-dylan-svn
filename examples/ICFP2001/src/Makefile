SOURCES=icfp2001.dylan icfp2001-exports.dylan

icfp2001: icfp2001.lid $(SOURCES)
	d2c -s icfp2001.lid

clean:
	rm -f *.o *.s *.a *.c *.mak *~ icfp2001
	rm -rf .libs

../runme: icfp2001
	cp icfp2001 ../runme
	strip ../runme

install: ../runme
	cd ..; tar czf dylan-hackers.tgz README buildme runme \
           src/Makefile src/icfp2001.lid src/*.dylan

test: icfp2001
	(time ./icfp2001 <tests/test2) && echo **Success**
