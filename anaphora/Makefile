VERSION=$(shell d2c --version | grep d2c | sed 's/d2c (Gwydion Dylan)//' | sed 's/ //')
MACHINE=$(shell uname -m)
ifeq ("$(MACHINE)", "sparc64")
	MACHINE=sparc
else
	ifeq ("$(MACHINE)", "i686")
		MACHINE=x86
	endif
endif

anaphora.lib.du: \
	anaphora.lid \
	anaphora.dylan \
	library.dylan
	d2c anaphora.lid	

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ anaphora.lib.du
	-rm -rf .libs

install: anaphora.lib.du 
	libtool --mode=install /usr/bin/install -c libanaphora-dylan.la /usr/lib/dylan/$(VERSION)/$(MACHINE)-linux-gcc/libanaphora-dylan.la
	libtool --finish /usr/lib/dylan/2.3.10pre3/$(MACHINE)-linux-gcc/dylan-user
	/usr/bin/install -c anaphora.lib.du `d2c --dylan-user-location`
