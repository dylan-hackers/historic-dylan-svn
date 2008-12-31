module: dylan-user

define library doctower
   use dylan, import: { extensions };
   use common-dylan;
   use command-line-parser;
   use string-extensions;
   use collection-extensions;
   use collections, import: { table-extensions };
   use regular-expressions;
   use system;
   use io;
   use peg-parser;
   use wrapper-streams;
   use dynamic-binding;
   // from Monday project
   use source-location;
end library;
