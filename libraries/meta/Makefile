INSTALLPATH=$(shell d2c --compiler-info | grep D2C_RUNTIME_SUBDIR | sed 's/_DCI_D2C_RUNTIME_SUBDIR=//' | sed s'/ //g')

FILES := library \
	 meta-base \
	 with-collector \
	 meta-types \
	 meta-syntax \
	 meta

meta.lib.du: $(FILES:.dylan)
	d2c meta.lid

clean:
	rm *.o *.c *~ cc-*.mak *.a *.du
	rm -r .libs

install: meta.lib.du 
	libtool --mode=install /usr/bin/install -c libmeta-dylan.la /usr/lib/dylan/$(INSTALLPATH)/libmeta-dylan.la
	libtool --finish /usr/lib/dylan/$(INSTALLPATH)/dylan-user
	/usr/bin/install -c meta.lib.du /usr/lib/dylan/$(INSTALLPATH)/meta.lib.du
