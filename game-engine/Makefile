vrml-viewer: vrml-viewer.lid
	d2c -L../examples/gdgl $<

vrml-viewer.lid: vrml-viewer-exports.dylan vrml-viewer.dylan indexed-face-set.dylan node.dylan gettimeofday.dylan errno.dylan
	touch $@

gettimeofday.dylan: gettimeofday.intr
	melange --d2c -I`gcc --print-file-name=include` gettimeofday.intr gettimeofday.dylan

errno.dylan: errno.intr
	melange --d2c -I`gcc --print-file-name=include` errno.intr errno.dylan


clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* vrml-viewer *~
