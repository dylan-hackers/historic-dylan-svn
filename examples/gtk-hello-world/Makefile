C_INCLUDE_PATH=`gtk-config --cflags | sed 's/-I//g; s/ /:/g'`

gtk-hello-world: gtk-hello-world.lid
	C_INCLUDE_PATH="${C_INCLUDE_PATH}" d2c $<

gtk-hello-world.lid: gtk-hello-world-exports.dylan gtk-intr.dylan gtk-hello-world.dylan
	touch $@

gtk-intr.dylan: gtk-intr.intr
	melange -d2c `gtk-config --cflags` -I/usr/lib/gcc-lib/i486-linux/2.7.2.1/include/ -Iinclude $< $@
