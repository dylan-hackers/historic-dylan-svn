xml-parser.lib.du: xml-parser.lid xml-parser.dylan xml-parser-exports.dylan gnome-xml.dylan
	d2c xml-parser.lid

%.dylan: %.intr standard-defines.h
	melange --d2c -I`gcc -print-file-name=include` -I/usr/local/include $< ,$@ && mv ,$@ $@

clean:
	-rm *.o *.s *.a *.c *.mak *~ xml-parser.lib.du

install: xml-parser.lib.du 
	/usr/bin/install -c libxml-parser.a xml-parser.lib.du `d2c --dylan-user-location`
