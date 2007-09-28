Module: dylan-user
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: D-databases-sql-odbc-test!library.dylan(trunk.4) $

define library sql-odbc-test
  use common-dylan;
  use threads;
  use sql-odbc; 

  use io;
  use system;
  use melange-support;

  use testworks;

  export sql-odbc-test;
end library;

define module sql-odbc-test-include
  create
    *datasource-name*,
    *user-name*,
    *user-password*,
    *the-dbms*,
    *detect-null-column*,
    *do-introspection*,
    *dbms-class*,
    *dbms-class-name*,
    *dbms-user-class*,
    *dbms-database-class*,
    *dbms-sql-statement-class*;
end module;

define module sql-odbc-test
  use common-dylan,           // common-dylan lib
    exclude: { format-to-string };
  use threads;                // threads lib
  use sql-odbc;               // sql-odbc lib

  use format-out;             // io lib
  use format;                 // io lib
  use date;                   // system lib
  use operating-system;       // system lib
  use melange-support;        // melange-support lib

  use testworks;              // testworks lib
  use sql-odbc-test-include;  
end module;

