bin:
	d2c ants.lid

lib:
	d2c ants-library.lid

vis:	lib
	cd Visualizer; make; cd ..

run:
	rm -rf dump.tiny
	./ants ant-brains/sample.ant ant-brains/sample.ant worlds/tiny.world > dump.tiny

run-vis:
	./Visualizer/ant-visualizer ant-brains/sample.ant ant-brains/sample.ant worlds/tiny.world

clean:
	-rm -f *.o *.lo *.s *.a *.c *.mak *~ ants
	-rm -rf .libs
	cd Visualizer; make clean; cd ..
