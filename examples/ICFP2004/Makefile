bin:
	d2c -M ants.lid

lib:
	d2c -M ants-library.lid

vis:	lib
	$(MAKE) -C Visualizer

run:
	rm -rf dump.tiny
	./ants ant-brains/sample.ant ant-brains/sample.ant worlds/tiny.world > dump.tiny

run-vis:
	./Visualizer/ant-visualizer ant-brains/sample.ant ant-brains/sample.ant worlds/sample0.world

clean:
	-rm -f *.o *.lo *.s *.a *.c *.mak *~ ants
	-rm -rf .libs
	$(MAKE) -C Visualizer clean

-include ants.deps
-include ants-library.deps
