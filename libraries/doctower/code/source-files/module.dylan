module: dylan-user

define module source-files
   use common;
   use conditions;
   use ordered-tree;
   use internal-rep;
   use markup-parser, import: { parse-markup };
   use dylan-parser, import: { parse-dylan-file };
   use markup-translator;
   
   // from regular-expressions
   use regular-expressions, import: { regexp-position };
   // from wrapper-streams
   use canonical-text-stream;
   
   export topics-from-markup-file, topics-from-dylan-file, toc-from-file;
end module;
