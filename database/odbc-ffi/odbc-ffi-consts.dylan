module: odbc-ffi
author: Dustin Voss
synopsis: Melange doesn't handle these types properly, so I did them by hand.

define constant $SQL-NULL-HENV = as(<SQLHANDLE>, 0);
define constant $SQL-NULL-HDBC = as(<SQLHANDLE>, 0);
define constant $SQL-NULL-HSTMT = as(<SQLHANDLE>, 0);
define constant $SQL-NULL-HDESC = as(<SQLHANDLE>, 0);
define constant $SQL-NULL-HANDLE = as(<SQLHANDLE>, 0);

define constant <intval> = <anonymous-379>; /* SQL_INTERVAL_STRUCT.intval */

define functional class <SQLUINTEGER*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<SQLUINTEGER*>));

define inline method pointer-value
    (ptr :: <SQLUINTEGER*>, #key index = 0)
 => (result :: <SQLUINTEGER>);
  unsigned-long-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <SQLUINTEGER>, ptr :: <SQLUINTEGER*>, #key index = 0)
 => (result :: <SQLUINTEGER>);
  unsigned-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<SQLUINTEGER*>)) => (result :: <integer>);
  4;
end method content-size;

