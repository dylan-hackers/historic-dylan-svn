LIBPATHS = -L ../odbc-ffi -L ../sql-odbc
LIBS = ../odbc-ffi/odbc-ffi.lib.du \
       ../sql-odbc/sql-odbc.lib.du

sql-odbc-test: sql-odbc-test.lid *.dylan $(LIBS)
	d2c --debug $(LIBPATHS) sql-odbc-test.lid

clean:
	-rm -rf *.o *.s *.a *.c *.lib.du *.mak *.el *~ sql-odbc-test
