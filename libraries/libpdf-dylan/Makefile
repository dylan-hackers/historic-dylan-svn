pdf.lib.du: pdf.lid objects.dylan constants.dylan arrays.dylan dictionaries.dylan streams.dylan documents.dylan graphics.dylan pdf-exports.dylan
	d2c pdf.lid
	touch ./test/test-exports.dylan
	make -C test

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ pdf.lib.du
	-rm -rf .libs

install: pdf.lib.du 
	 /usr/bin/install -c libpdf-dylan.a pdf.lib.du `d2c --dylan-user-location`
