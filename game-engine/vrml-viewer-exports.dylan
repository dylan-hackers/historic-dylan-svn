module: dylan-user
author: Andreas Bogk <andreas@andreas.org>
copyright: (C) Andreas Bogk, under LGPL

define library vrml-viewer
  use dylan;
  use common-dylan;
  use io;
  use garbage-collection;
  use opengl;
  use melange-support;

//  use melange-support // for null-pointer
end library;

define module gettimeofday
  use dylan;
  use melange-support;
  use format-out;
  use standard-io;
  use streams;

  export current-time;
end module gettimeofday;

define module vrml-viewer
  use common-dylan;
  use extensions, exclude: { assert };
  use standard-io;
  use system;
  use streams;
  use format;
  use format-out;
  use garbage-collection;

  use opengl;
  use opengl-glu;
  use opengl-glut;
  use gettimeofday;

//  use melange-support // for null-pointer
end module;
