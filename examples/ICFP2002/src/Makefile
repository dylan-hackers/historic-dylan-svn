SOURCES=icfp2002.dylan icfp2002-exports.dylan

icfp2002: icfp2002.lid $(SOURCES)
	d2c -s icfp2002.lid

clean:
	rm -f *.o *.s *.a *.c *.mak *~ icfp2002
	rm -rf .libs


install: icfp2002
	strip icfp2002
	cd ..; tar czf dylan-hackers.tgz README buildme runme \
           src/Makefile src/icfp2002.lid src/*.dylan src/icfp2002
	   md5sum ../dylan-hackers.tgz

test: icfp2002
	(time ./icfp2002) && echo **Success**
