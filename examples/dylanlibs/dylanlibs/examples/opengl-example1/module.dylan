Module:    dylan-user
Synopsis:  Example OpenGL Project
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.

define module opengl-example1
  use dylan;
  use duim;

  use c-ffi;
  use win32-duim;
  use win32-common, exclude: { <point> };
  use win32-gdi, exclude: { <pattern> };
  use win32-user;
  use win32-gl;

  // Add binding exports here.

end module opengl-example1;
