module: dylan-user

define module common-imports  
   // from dylan
   use extensions, import: { <set> }, export: all;
   // from common-dylan
   use dylan, export: all;
   use common-extensions, export: all,
      exclude: { format-to-string };
   use simple-profiling, import: { profiling }, export: all;
   // from collections
   use table-extensions,
      rename: { case-insensitive-equal => case-insensitive-equal? },
      export: all;
   // from collection-extensions
   use collection-utilities, export: all;
   use sequence-utilities, export: { partition };
   // from system
   use file-system, import: { <file-stream>, stream-locator }, export: all;
   use locators, import: { <file-locator>, <directory-locator> }, export: all;
   // from io
   use streams,
      exclude: { <sequence-stream>, <string-stream>, <byte-string-stream> },
      export: all;
   use format, export: all;
   use standard-io, export: all;
   use print, import: { print-object, print }, export: all;
   use pprint, import: { printing-logical-block, pprint-newline }, export: all;
   // from sequence-stream
   use sequence-stream, export: all;
   // from regular-expressions
   use regular-expressions, import: { join }, export: all;
   // from source-location
   use source-location, export: all;
   // from dynamic-binding
   use dynamic-binding, export: all;
   // from skip-list
   use skip-list,
      import: { <skip-list>, element-sequence, element-sequence-setter },
      export: all;
   // from slot-visitor
   use slot-visitor, export: all;
end module;
