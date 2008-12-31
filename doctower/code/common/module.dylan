module: dylan-user

define module common
   // from dylan
   use extensions, import: { report-condition, <set> }, export: all;
   // from common-dylan
   use dylan, export: all;
   use common-extensions, export: all,
      exclude: { format-to-string };
   // from collections
   use table-extensions,
      rename: { case-insensitive-equal => case-insensitive-equal? },
      export: all;
   // from collection-extensions
   use collection-utilities, export: all;
   use sequence-utilities, export: { partition };
   // from system
   use file-system, import: { <file-stream>, stream-locator }, export: all;
   use locators, import: { <file-locator>, locator-extension }, export: all;
   // from io
   use streams, export: all;
   use format, export: all;
   use standard-io, export: all;
   use print, import: { print-object, print }, export: all;
   use pprint, import: { printing-logical-block, pprint-newline }, export: all;
   // from regular-expressions
   use regular-expressions, import: { join }, export: all;
   // from source-location
   use source-location, export: all;
   // from dynamic-binding
   use dynamic-binding, export: all;
   
   export log, log-object, slot-visitor-definer, merge-file-source-locations,
          item-string-list;
end module;

