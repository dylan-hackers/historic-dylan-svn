icfp2000: icfp2000.lid icfp2000.dylan icfp2000-exports.dylan matrix.dylan
	d2c -s icfp2000.lid

clean:
	rm -f *.o *.s *.a *.c *.mak *~ icfp2000
	rm -rf .libs

../runme: icfp2000
	cp icfp2000 ../runme
	strip ../runme

install: ../runme
	cd ..; tar czf ../dylan-hackers.tar.gz README buildme runme \
           src/Makefile src/icfp2000.lid src/*.dylan
