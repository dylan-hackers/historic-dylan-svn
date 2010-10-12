module: dylan-user

define module source-files
   use common;
   use conditions;
   use ordered-tree;
   use markup-rep;
   use parser-common,
      import: { canonical-text-stream, line-col-position-func,
                source-location => token-src-loc };
   use markup-parser,
      import: { parse-markup, <markup-content-token> };
   use dylan-parser,
      import: { parse-dylan, <interchange-file-token>, <header-token>, headers,
                hdr-keyword, hdr-value };
   use markup-translator;
   use dylan-translator;
   use dylan-topics;
   
   // from regular-expressions
   use regular-expressions, import: { regexp-position };
   // from system
   use file-system, import: { <file-does-not-exist-error> };
   use locators, import: { locator-extension, merge-locators };
   
   export topics-from-markup-files, topics-from-dylan-files, toc-from-file;
end module;
