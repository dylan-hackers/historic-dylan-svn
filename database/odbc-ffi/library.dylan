module: dylan-user
author: Dustin Voss
synopsis: The declaration for the ODBC library. The modules are declared in
          the module*.dylan files.

define library odbc-ffi
  use dylan, import: { dylan, extensions };
  use melange-support, export: all;
    
  export odbc-ffi;
end library;
