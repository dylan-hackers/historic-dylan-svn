ifeq ($(OSTYPE), darwin)
	LIDFILE=sdl-macosx.lid
 	MELANGE=melange --framework SDL --framework OpenGL --d2c
else
	LIDFILE=sdl.lid
	MELANGE=melange -I/usr/local/sdl/include --d2c
endif

sdl.lib.du: $(LIDFILE) sdl.dylan
ifeq ($(OSTYPE), darwin)
	cc -c macosx-main/SDLMain.m -o macosx-main/SDLMain.o
endif
	d2c $(LIDFILE)

sdl.lid: sdl-exports.dylan sdl-intr.dylan sdl-glut-intr.dylan
	touch $@

sdl.dylan: sdl.intr
	$(MELANGE) --module-file sdl-wrappers-module.dylan sdl.intr sdl.dylan

clean:
	-rm -f *.mak *.lib.du *.o macosx-main/*.o *.a *.s *.c ,* *~ \
	sdl.dylan module.dylan
