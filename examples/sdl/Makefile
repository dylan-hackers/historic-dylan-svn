ifeq ($(OSTYPE), darwin)
	LIDFILE=macosx-sdl.lid
 	MELANGE=melange --framework SDL --framework OpenGL --d2c
else
	LIDFILE=sdl.lid
	MELANGE=melange -I/usr/local/sdl/include --d2c
endif

sdl.lib.du: $(LIDFILE) sdl-exports.dylan sdl.dylan 
	d2c $(LIDFILE)

sdl.lid: sdl-exports.dylan sdl-intr.dylan sdl-glut-intr.dylan
	touch $@

sdl.dylan: sdl.intr
	$(MELANGE) sdl.intr > sdl.dylan

clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* *~ \
	sdl.dylan 
