module: dylan-user

define library icfp2000
  use common-dylan;
  use io;
  use transcendental;
  use matrix;
end library;

define module GML-compiler
  use Dylan;
  use Transcendental;
  export compile-GML, run-GML;
end module GML-compiler;


define module icfp2000
  use common-dylan;
  use format-out;
  use streams;
  use format;
  use print;
  use standard-io;
  use transcendental;
  use matrix;
  use Extensions,
    import: {<extended-integer>, ratio, <false>};
  use GML-compiler;
end module;
