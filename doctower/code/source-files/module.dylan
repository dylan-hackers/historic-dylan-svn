module: dylan-user

define module source-files
   use common;
   use conditions;
   use ordered-tree;
   use internal-rep;
   use markup-parser, import: { parse-markup };
   use dylan-parser, import: all, exclude: { source-location };
   use markup-translator;
   
   // from regular-expressions
   use regular-expressions, import: { regexp-position };
   // from wrapper-streams
   use canonical-text-stream;
   // from system
   use file-system, import: { <file-does-not-exist-error> };
   use locators, import: { locator-extension, merge-locators };
   
   export topics-from-markup-files, topics-from-dylan-files, toc-from-file;
end module;
