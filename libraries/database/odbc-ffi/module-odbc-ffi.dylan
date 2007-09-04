module: dylan-user
author: Dustin Voss
synopsis: The C-FFI interface to the ODBC library, created by Melange and by
          hand. This interface is very similar to Open Dylan's.

define module odbc-ffi
  use dylan;
  use melange-support;
  use odbc-ffi-melange, export: all;

  export
    $SQL-NULL-HANDLE,
    $SQL-NULL-HDBC,
    $SQL-NULL-HDESC,
    $SQL-NULL-HENV,
    $SQL-NULL-HSTMT,
    <intval>
end module;

  