Module:       dylan-user
Author:       Carl Gay

define library btrack
  use common-dylan;
  use system;        // for date module
  use io;
  use sql-odbc;
  use koala;
  export btrack;
end;

define module btrack
  use common-dylan;
  use date;
  use format;
  use streams;
  use threads;
  use sql-odbc,
    prefix: "sql$";
  use dsp;            // Dylan Server Pages, exported from Koala
end;
