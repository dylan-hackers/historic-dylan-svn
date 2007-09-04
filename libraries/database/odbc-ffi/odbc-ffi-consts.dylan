module: odbc-ffi
author: Dustin Voss
synopsis: Melange doesn't handle these types properly, so I did them by hand.

define constant $SQL-NULL-HENV = as(<SQLHANDLE>, 0);
define constant $SQL-NULL-HDBC = as(<SQLHANDLE>, 0);
define constant $SQL-NULL-HSTMT = as(<SQLHANDLE>, 0);
define constant $SQL-NULL-HDESC = as(<SQLHANDLE>, 0);
define constant $SQL-NULL-HANDLE = as(<SQLHANDLE>, 0);

define constant <intval> = <anonymous-379>; /* SQL_INTERVAL_STRUCT.intval */
