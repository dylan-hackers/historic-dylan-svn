Module: dylan-user
Author: Dustin Voss
Synopsis: This library is a Gwydion Dylan implementation of the Open Dylan
          sql and sql-odbc libraries. See subdirectories for module decl's.

define library sql-odbc
  use common-dylan, import: { dylan, common-extensions };
  use dylan, import: { system };
  use system, import: { date };
  use table-extensions;
  use threads;
  use odbc-ffi;
  use io;
	
  export sql-odbc;
end library;
