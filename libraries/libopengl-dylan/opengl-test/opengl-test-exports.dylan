library: opengl-test
module: dylan-user
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

define library opengl-test
  use dylan;
  use common-dylan;
  use io;
  use garbage-collection;
  use opengl;

//  use melange-support // for null-pointer
end library;

define module opengl-test
  use common-dylan;
  use extensions;
  use streams;
  use system, import: {callback-method};
  use format, exclude: {format-to-string};
  use print;
  use standard-io;
  use garbage-collection;

  use opengl;
  use opengl-glu;
  use opengl-glut;

//  use melange-support // for null-pointer
end module;
