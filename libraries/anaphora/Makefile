anaphora.lib.du: \
	anaphora.lid \
	anaphora.dylan \
	library.dylan
	d2c anaphora.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ anaphora.lib.du
	-rm -rf .libs

install: anaphora.lib.du 
	libtool /opt/sfw/bin/ginstall -c libanaphora.a anaphora.lib.du `d2c --dylan-user-location`
