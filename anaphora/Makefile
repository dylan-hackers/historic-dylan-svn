INSTALLPATH=$(shell d2c --compiler-info | grep D2C_RUNTIME_SUBDIR | sed 's/_DCI_D2C_RUNTIME_SUBDIR=//' | sed s'/ //g

anaphora.lib.du: \
	anaphora.lid \
	anaphora.dylan \
	library.dylan
	d2c anaphora.lid	

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ anaphora.lib.du
	-rm -rf .libs

install: anaphora.lib.du 
	libtool --mode=install /usr/bin/install -c libanaphora-dylan.la /usr/lib/dylan/$(INSTALLPATH)/libanaphora-dylan.la
	libtool --finish /usr/lib/dylan/$(INSTALLPATH)/dylan-user
	/usr/bin/install -c anaphora.lib.du $(INSTALLPATH)/anaphora.lib.du
