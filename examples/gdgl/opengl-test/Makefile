opengl-test: opengl-test.lid
	d2c -L.. $<

opengl-test.lid: opengl-test-exports.dylan opengl-test.dylan
	touch $@

clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* opengl-test *~
