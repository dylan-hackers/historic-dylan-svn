library: opengl-test
module: dylan-user
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

define library opengl-test
  use dylan;
  use streams;
  use format;
  use print;
  use standard-io;
  use garbage-collection;
  use opengl;

//  use melange-support // for null-pointer
end library;

define module opengl-test
  use dylan;
  use extensions;
  use system;
  use streams;
  use format;
  use print;
  use standard-io;
  use garbage-collection;

  use opengl;
  use opengl-glu;
  use opengl-glut;

//  use melange-support // for null-pointer
end module;
