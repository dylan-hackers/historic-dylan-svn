module: dylan-user

define library icfp2000
  use common-dylan;
  use io;
  use transcendental;
  use matrix;
end library;


define module gml-lexer
  use Dylan;
  use common-dylan;
  use standard-io;
  use streams;
  use format;
  use Extensions,
    import: {<extended-integer>, ratio, <false>};

  export lex-gml;
end module gml-lexer;


define module gml-compiler
  use Dylan;
  use Transcendental;

  export compile-gml, run-gml, <fp>;
end module gml-compiler;


define module icfp2000
  use common-dylan;
  use format-out;
  use format;
  use streams;
  use print;
  use standard-io;
  use transcendental;
  use matrix;

  use gml-lexer;
  use gml-compiler;
end module;
