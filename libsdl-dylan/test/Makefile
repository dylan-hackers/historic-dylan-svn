test: test.lid test.dylan test-exports.dylan
	d2c -L.. test.lid
ifeq ($(OSTYPE), darwin)
	../macosx-main/compose-cocoa-app test
endif

clean:
	-rm -f *.o *.s *.a *.c *.mak  *~ test
	-rm -rf .libs *.app *.app.old
