icfp2000: icfp2000.lid icfp2000.dylan icfp2000-exports.dylan
	d2c -s icfp2000.lid

clean:
	rm -f *.o *.s *.a *.c *.mak *~ icfp2000
	rm -rf .libs

../runme: icfp2000
	cp icfp2000 ../runme
	strip ../runme

install: ../runme clean
	cd ..; tar zcf ../dylan-hackers.tar.gz *
