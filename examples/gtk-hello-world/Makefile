C_INCLUDE_PATH=`gtk-config --cflags | sed 's/-I//g; s/ /:/g'`

gtk-hello-world: gtk-hello-world.lid gtk-base.lib.du
	d2c $<

gtk-base.lib.du: gtk-base.lid
	C_INCLUDE_PATH="${C_INCLUDE_PATH}" d2c gtk-base.lid

gtk-base.lid: gtk-base-exports.dylan gtk-intr.dylan

gtk-hello-world.lid: gtk-hello-world-exports.dylan gtk-hello-world.dylan
	touch $@

gtk-intr.dylan: gtk-intr.intr
	melange -v --d2c `gtk-config --cflags` -I`gcc -print-file-name=include` $< ,$@ && mv ,$@ $@

clean:
	-rm *.o *.a *.s *.c ,* *.map *temp gtk-hello-world *~ gtk-intr.dylan
