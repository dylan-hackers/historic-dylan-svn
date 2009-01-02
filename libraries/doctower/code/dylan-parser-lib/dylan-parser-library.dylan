module: dylan-user

define library dylan-parser-library
   use support-library;
   use peg-parser;
   use string-extensions;
   use wrapper-streams;
   export dylan-parser;
end library;