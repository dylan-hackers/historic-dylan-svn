INSTALLPATH=$(shell d2c --compiler-info | grep _DCI_D2C_DYLAN_USER_DIR | sed 's/_DCI_D2C_DYLAN_USER_DIR=//' | sed 's/\/dylan-user//' | sed s'/ //g')

ifeq ($(shell if [ -d "/System/Library/Frameworks" ]; then echo -n darwin; else echo -n not-darwin; fi), darwin)
	LIDFILE=macosx-opengl.lid
	MELANGE=melange --framework GLUT --d2c
else
ifeq ($(shell uname), NetBSD)
	LIDFILE=opengl.lid
	MELANGE=melange -I/usr/pkg/include -I/usr/X11R6/include --d2c
else
	LIDFILE=opengl.lid
	MELANGE=melange -I/usr/X11R6/include --d2c
endif
endif

opengl.lib.du: $(LIDFILE) opengl-exports.dylan opengl-intr.dylan \
               opengl-glut-intr.dylan opengl-glu-intr.dylan \
	       opengl-macros.dylan 
#              opengl-osmesa-intr.dylan
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
	-rm -f *.mak *.lib.du *.o *.lo *.la *.a *.s *.c ,* *~ \
	opengl-intr.dylan opengl-glu-intr.dylan opengl-glut-intr.dylan \
	opengl-osmesa-intr.dylan
	-rm -rf .libs

install: opengl.lib.du 
	libtool --mode=install /usr/bin/install -c libopengl-dylan.la $(INSTALLPATH)/libopengl-dylan.la
	libtool --finish $(INSTALLPATH)
	/usr/bin/install -c opengl.lib.du $(INSTALLPATH)/opengl.lib.du
