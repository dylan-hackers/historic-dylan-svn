Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  A Dylan application to play around in
Copyright: 1997 Harlequin Group plc. All rights reserved.

define module dylan-playground
  use common-dylan,
    //---*** This shouldn't be necessary!
    exclude: { format-to-string };
  use simple-random;
  use threads;
  use finalization;
  use machine-integer-user;

  use operating-system;
  use date;
  use file-system;
  use locators;

  use streams;
  use standard-io;
  use print;
  use format;
  use format-out;

  use bit-vector;
  use bit-set;
  use byte-vector;
  use set;
  use table-extensions;

  use duim;
end module dylan-playground;
