module: dylan-user
copyright: 1996 The Harlequin Group Limited. All rights reserved.

define library harlequin-dylan
  use dylan, export: all;
  use harlequin-extensions, export: all;
  use machine-word, export: all;
  use byte-vector, export: all;
  use transcendentals, export: all;
  use threads, export: all;
  export harlequin-dylan;
end library;

define module harlequin-dylan
  use dylan, export: all;
  use harlequin-extensions, export: all;
end module;

// eof
