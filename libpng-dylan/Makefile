libpng.lib.du: libpng.dylan libpng-intr.dylan libpng.lid libpng-internal-exports.dylan libpng-exports.dylan
	d2c libpng.lid

libpng-intr.dylan: libpng-intr.intr
	melange --d2c -I`gcc --print-file-name=include` -m libpng-internal-exports.dylan, libpng-intr.intr libpng-intr.dylan,
	mv libpng-intr.dylan, libpng-intr.dylan
	mv libpng-internal-exports.dylan, libpng-internal-exports.dylan
