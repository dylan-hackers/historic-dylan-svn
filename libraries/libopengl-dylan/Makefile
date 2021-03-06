INSTALLPATH=$(shell d2c --compiler-info | grep _DCI_D2C_DYLAN_USER_DIR | sed 's/_DCI_D2C_DYLAN_USER_DIR=//' | sed 's/\/dylan-user//' | sed s'/ //g')

LIBTOOL=libtool

ifeq ($(shell if [ -d "/System/Library/Frameworks" ]; then echo -n darwin; else echo -n not-darwin; fi), darwin)
	LIDFILE=macosx-opengl.lid
	MELANGE=melange --framework OpenGL --framework GLUT --d2c
else
ifeq ($(shell uname -o), Cygwin)
	LIDFILE=cygwin-opengl.lid
	MELANGE=melange -v --headers -I/usr/include/w32api -I`$(CC) -print-file-name=include` --d2c
# -DUSE_OPENGL32
	LIBTOOL=touch configure.in && libtool
else
ifeq ($(shell uname), NetBSD)
	LIDFILE=opengl.lid
	MELANGE=melange -I/usr/pkg/include -I/usr/X11R6/include --d2c
else
	LIDFILE=opengl.lid
	MELANGE=melange -I/usr/X11R6/include --d2c
endif
endif
endif

opengl.lib.du: $(LIDFILE) \
		opengl-exports.dylan \
		opengl-intr.dylan \
		opengl-glut-intr.dylan \
		opengl-glu-intr.dylan \
		opengl-macros.dylan 
	d2c $(LIDFILE)

opengl.lid: opengl-exports.dylan opengl-intr.dylan opengl-glut-intr.dylan opengl-glu-intr.dylan opengl-macros.dylan
	touch $@

opengl-intr.dylan: opengl.intr
	$(MELANGE) $< ,$@ \
	&& sed 's/\$$GLenum\$$/$$/g' < ,$@ > $@ && rm ,$@

opengl-glu-intr.dylan: opengl-glu.intr
	$(MELANGE) $< ,$@ && mv ,$@ $@

opengl-glut-intr.dylan: opengl-glut.intr
	$(MELANGE) $< ,$@ && mv ,$@ $@

clean:
	-rm -f *.mak *.lib.du *.o *.lo *.la *.a *.s *.c ,* *~ \
	opengl-intr.dylan opengl-glu-intr.dylan opengl-glut-intr.dylan \
	opengl-osmesa-intr.dylan
	-rm -rf .libs
	-rm -rf *.stackdump

install: opengl.lib.du 
	$(LIBTOOL) --mode=install /usr/bin/install -c libopengl-dylan.la $(INSTALLPATH)/libopengl-dylan.la
	$(LIBTOOL) --finish $(INSTALLPATH)
	/usr/bin/install -c opengl.lib.du $(INSTALLPATH)/opengl.lib.du
