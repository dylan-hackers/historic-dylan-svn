Module:    dylan-user
Synopsis:  Example OpenGL Project
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.

define library opengl-example1
  use dylan;
  use duim;
  use c-ffi;

  use win32-duim;
  use win32-common;
  use win32-gdi;
  use win32-user; // for GetDC
  use win32-gl;
  use win32-glu;

  // Add any more module exports here.
  export opengl-example1;
end library opengl-example1;
