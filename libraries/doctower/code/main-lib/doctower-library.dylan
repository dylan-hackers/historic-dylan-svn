module: dylan-user

define library doctower-library
   use support-library;
   use template-engine;
   use string-extensions, import: { character-type };
   use dylan-parser-library;
   use markup-parser-library;
   use dylan-rep-library;
   use peg-parser;
   use system;
   use regular-expressions;
   use wrapper-streams;
   use io;
   use command-line-parser;
   use dylan;
end library;