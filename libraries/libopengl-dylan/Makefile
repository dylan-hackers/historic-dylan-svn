INSTALLPATH=$(shell d2c --compiler-info | grep D2C_RUNTIME_SUBDIR | sed 's/_DCI_D2C_RUNTIME_SUBDIR=//' | sed s'/ //g')

ifeq ($(OSTYPE), darwin)
	LIDFILE=macosx-opengl.lid
	MELANGE=melange --framework GLUT --d2c
else
	LIDFILE=opengl.lid
	MELANGE=melange -I/usr/X11R6/include --d2c
endif

opengl.lib.du: $(LIDFILE) opengl-exports.dylan opengl-intr.dylan \
               opengl-glut-intr.dylan opengl-glu-intr.dylan \
	       opengl-macros.dylan opengl-osmesa-intr.dylan
	d2c $(LIDFILE)

opengl.lid: opengl-exports.dylan opengl-intr.dylan opengl-glut-intr.dylan
	touch $@

opengl-intr.dylan: opengl.intr
	$(MELANGE) $< ,$@ \
	&& sed 's/\$$GLenum\$$/$$/g' < ,$@ > $@ && rm ,$@

opengl-glu-intr.dylan: opengl-glu.intr
	$(MELANGE) $< ,$@ && mv ,$@ $@

opengl-glut-intr.dylan: opengl-glut.intr
	$(MELANGE) $< ,$@ && mv ,$@ $@

opengl-osmesa-intr.dylan: opengl-osmesa.intr
	$(MELANGE) $< ,$@ && mv ,$@ $@

clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* *~ \
	opengl-intr.dylan opengl-glu-intr.dylan opengl-glut-intr.dylan \
	opengl-osmesa-intr.dylan

install: opengl.lib.du 
	libtool --mode=install /usr/bin/install -c libopengl-dylan.la /usr/lib/dylan/$(INSTALLPATH)/libopengl-dylan.la
	libtool --finish /usr/lib/dylan/$(INSTALLPATH)/dylan-user
	/usr/bin/install -c opengl.lib.du /usr/lib/dylan/$(INSTALLPATH)/opengl.lib.du
