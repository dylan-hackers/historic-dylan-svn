Module:    dylan-user
Synopsis:  We want to see graphs
Author:    Andreas Bogk, Hannes Mehnert
Copyright: (C) 2005,  All rights reserved.

define library graph-viewer
  use functional-dylan;
  use common-dylan, import: { simple-random, transcendentals };
  use duim;
  use duim-extended-geometry;
  use io;
  use system;
  use c-ffi;
  use win32-common;
  use win32-controls;
  use win32-dialog;
  use win32-gdi;
  use win32-user;
  use win32-gl;
  use win32-glu;

  // Add any more module exports here.
  export graph-viewer;
end library graph-viewer;
