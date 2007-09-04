MODULES = module-odbc-ffi.dylan \
          module-odbc-ffi-melange.dylan

odbc-ffi.lib.du: odbc-ffi.lid $(MODULES)
	d2c odbc-ffi.lid

module-odbc-ffi-melange.dylan: odbc-ffi.intr
	melange --d2c -m $@ $? odbc-ffi-melange.dylan

clean:
	-rm -f *.o *.s *.a *.c *.mak *.el *~ odbc-ffi.lib.du
	-rm -rf .libs
	-rm -f *-melange.dylan
