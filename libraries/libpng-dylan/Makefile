libpng.lib.du: libpng.dylan libpng-intr.dylan libpng.lid
	d2c libpng.lid

libpng-intr.dylan: libpng-intr.intr
	melange --d2c -I`gcc --print-file-name=include` libpng-intr.intr libpng-intr.dylan,
	mv libpng-intr.dylan, libpng-intr.dylan
