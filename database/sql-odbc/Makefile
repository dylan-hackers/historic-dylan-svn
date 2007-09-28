SOURCES = sql/*.dylan \
          sql-odbc/*.dylan

LIBPATH = ../odbc-ffi

sql-odbc.lib.du: sql-odbc.lid $(SOURCES)
	d2c --debug -L $(LIBPATH) sql-odbc.lid

clean:
	-rm -rf *.o *.s *.a *.c *.mak *.el *~ sql-odbc.lib.du
