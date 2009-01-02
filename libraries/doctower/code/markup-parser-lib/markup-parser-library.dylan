module: dylan-user

define library markup-parser-library
   use support-library;
   use peg-parser;
   use string-extensions;
   use wrapper-streams;
   use regular-expressions;

   export markup-parser;
end library;