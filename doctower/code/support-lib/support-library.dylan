module: dylan-user

define library support-library
   use dylan, import: { extensions };
   use common-dylan;
   use collection-extensions;
   use collections, import: { table-extensions };
   use regular-expressions;
   use system;
   use io;
   use sequence-stream;
   use peg-parser;
   use dynamic-binding;
   use skip-list;
   // from Monday project
   use source-location;

   export common, conditions, configs, parser-common, markup-rep, dylan-rep,
          ordered-tree;
end library;
