module: dylan-user

define library doctower-library
   use support-library;
   use dylan-parser-library;
   use markup-parser-library;
   use system;
   use regular-expressions;
   use wrapper-streams;
   use io;
   use command-line-parser;
   use dylan;
end library;