library: balls
module: dylan-user
author: brent Fulgham <bfulham@debian.org>
copyright: (C) Brent Fulgham (Terms: Public Domain)

define library balls
  use dylan;
  use streams;
  use format;
  use print;
  use standard-io;
  use garbage-collection;
  use transcendental;
  use opengl;
end library;

define module balls
  use dylan;
  use extensions;
  use system;
  use streams;
  use format;
  use print;
  use standard-io;
  use transcendental;
  use garbage-collection;

  use opengl;
  use opengl-glu;
  use opengl-glut;
end module;
