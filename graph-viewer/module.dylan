Module:    dylan-user
Synopsis:  We want to see graphs
Author:    Andreas Bogk, Hannes Mehnert
Copyright: (C) 2005,  All rights reserved.

define module graph-viewer
  use functional-dylan;
  use dylan-extensions;
  use threads;
  use transcendentals;
  use simple-random;
  use duim;
  use duim-internals, exclude: { position };
  use duim-extended-geometry;
  use format;
  use format-out;
  use standard-io;
  use streams;
  use date;
  use file-system;
  use operating-system;
  use c-ffi;
  use win32-duim;
  use win32-common;
  use win32-controls;
  use win32-dialog;
  use win32-gdi;
  use win32-user;
  use win32-gl;
  use win32-glu;

  // Add binding exports here.

end module graph-viewer;
