FILES := test-library \
	 entity-pass \
	 html-xform \
	 xml-test

xml-test: $(FILES:.dylan)
	d2c -L .. -L ../../meta xml-test.lid

clean:
	rm *.o *.c *~ cc-*.mak *.a xml-test

