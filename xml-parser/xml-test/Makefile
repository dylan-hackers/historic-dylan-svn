FILES := test-library.dylan \
	 entity-pass.dylan \
	 html-xform.dylan \
	 xml-test.dylan \
	 ../xml-parser.lib.du \
	 ../../meta/meta.lib.du

xml-test: $(FILES)
	d2c -L .. -L ../../anaphora -L ../../meta xml-test.lid

clean:
	rm *.o *.c *~ cc-*.mak *.a xml-test

