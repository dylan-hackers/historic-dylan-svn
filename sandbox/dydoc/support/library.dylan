module: dylan-user

define library dydoc
   use common-dylan;
   use command-line-parser;
   use string-extensions;
   use collection-extensions;
   use collections, import: { table-extensions };
   use regular-expressions;
   use system;
   use io;
   use peg-parser;
   // from Monday project
   use grammar, import: { digraph };
   use source-location;
end library;

define module common
   // from common-dylan
   use dylan, export: all;
   use common-extensions, export: all,
      exclude: { format-to-string };
   // from collections
   use table-extensions, export: all;
   // from collection-extensions
   use collection-utilities, export: all;
   // from io
   use streams, export: all;
   use format, export: all;
   use standard-io, export: all;
   use print, export: { print-object };
   // from system
   use file-system, export: all;
   // from regular-expressions
   use regular-expressions, import: { join }, export: all;
   
   export log, log-object, slot-visitor-definer;
end module;

