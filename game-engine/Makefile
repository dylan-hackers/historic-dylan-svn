go: vrml-viewer
	./vrml-viewer test-zentrale.wrl

vrml-viewer: vrml-viewer.lid ../examples/gdgl/opengl.lib.du ../examples/meta/meta.lib.du
	d2c -g -L../examples/gdgl -L../examples/meta $<

vrml-viewer.lid: \
  vrml-viewer-exports.dylan \
  vrml-viewer.dylan \
  render-to-opengl.dylan \
  node.dylan \
  vrml-parser.dylan \
  gettimeofday.dylan \
  errno.dylan \
  vector-math.dylan
	touch $@

gettimeofday.dylan: gettimeofday.intr
	melange --d2c -I`gcc --print-file-name=include` gettimeofday.intr gettimeofday.dylan

errno.dylan: errno.intr
	melange --d2c -I`gcc --print-file-name=include` errno.intr errno.dylan


clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* vrml-viewer *~
