SOURCES=icfp2000.dylan icfp2000-exports.dylan matrix.dylan obj.dylan \
	lexer.dylan ppm.dylan render.dylan ppm.dylan color.dylan \
	intersection.dylan Engine.dylan Math-Primitives.dylan \
	light.dylan graphics-primitives.dylan types.dylan

icfp2000: icfp2000.lid $(SOURCES)
	d2c -g -s icfp2000.lid

clean:
	rm -f *.o *.s *.a *.c *.mak *~ icfp2000
	rm -rf .libs

../runme: icfp2000
	cp icfp2000 ../runme
	strip ../runme

install: ../runme
	cd ..; tar czf ../dylan-hackers.tar.gz README buildme runme \
           src/Makefile src/icfp2000.lid src/*.dylan

test: icfp2000
	./icfp2000 <../examples/factorial.gml && xview render.ppm
