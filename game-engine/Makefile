go: vrml-viewer
	./vrml-viewer

vrml-viewer: vrml-viewer.lid ../examples/gdgl/opengl.lib.du
	d2c -L../examples/gdgl $<

vrml-viewer.lid: vrml-viewer-exports.dylan vrml-viewer.dylan render-to-opengl.dylan node.dylan gettimeofday.dylan errno.dylan
	touch $@

gettimeofday.dylan: gettimeofday.intr
	melange --d2c -I`gcc --print-file-name=include` gettimeofday.intr gettimeofday.dylan

errno.dylan: errno.intr
	melange --d2c -I`gcc --print-file-name=include` errno.intr errno.dylan


clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* vrml-viewer *~
