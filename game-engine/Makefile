vrml-viewer: vrml-viewer.lid
	d2c $<

vrml-viewer.lid: vrml-viewer-exports.dylan vrml-viewer.dylan indexed-face-set.dylan node.dylan
	touch $@

clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* vrml-viewer *~
