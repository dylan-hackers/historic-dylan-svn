MELANGE=melange -I/usr/X11R6/include --d2c

opengl.lib.du: opengl.lid opengl-exports.dylan opengl-intr.dylan \
               opengl-glut-intr.dylan opengl-glu-intr.dylan \
	       opengl-macros.dylan opengl-osmesa-intr.dylan
	d2c opengl.lid

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
