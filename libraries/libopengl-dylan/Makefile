opengl.lib.du: opengl.lid opengl-exports.dylan opengl-intr.dylan \
               opengl-glut-intr.dylan opengl-glu-intr.dylan \
	       opengl-macros.dylan opengl-osmesa-intr.dylan
	d2c opengl.lid

opengl.lid: opengl-exports.dylan opengl-intr.dylan opengl-glut-intr.dylan
	touch $@

opengl-intr.dylan: opengl.intr
	melange -I. -I/usr/X11R6/include --d2c $< ,$@ \
	&& sed 's/\$$GLenum\$$/$$/g' < ,$@ > $@ && rm ,$@

opengl-glu-intr.dylan: opengl-glu.intr
	melange -I. -I/usr/X11R6/include --d2c $< ,$@ && mv ,$@ $@

opengl-glut-intr.dylan: opengl-glut.intr
	melange -I. -I/usr/X11R6/include --d2c $< ,$@ && mv ,$@ $@

opengl-osmesa-intr.dylan: opengl-osmesa.intr
	melange -I. -I/usr/X11R6/include --d2c $< ,$@ && mv ,$@ $@

clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* opengl-test *~ \
	opengl-intr.dylan opengl-glu-intr.dylan opengl-glut-intr.dylan \
	opengl-osmesa-intr.dylan
