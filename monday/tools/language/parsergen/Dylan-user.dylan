Module: Dylan-user


define library parsergen
  use common-dylan;
  use io;
  use system;
  use grammar;
end library;
          
define module parsergen
  use common-dylan, exclude: { format-to-string };
  use file-system;
  use streams;
  use format;
  use standard-io;

  use grammar;
  use parser-automaton;
end module;
          
