library: dylan-gears
module: dylan-user
author: Brent Fulgham <bfulgham@debian.org>
copyright: (C) Brent Fulgham (Terms: Public Domain)

define library dylan-gears
  use dylan;
  use streams;
  use format;
  use print;
  use standard-io;
  use garbage-collection;
  use transcendental;
  use opengl;
end library;

define module dylan-gears
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
