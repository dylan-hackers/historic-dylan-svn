module: dylan-user
synopsis: This module converts from tokens to intermediate markup (the DITA-style
          markup used internally).

define module markup-translator
   use common;
   use markup-parser,
      rename: { content => token-content, index => token-index };
   use internal-rep;
   use ordered-tree;
   use configs;
   
   // from collection-extensions
   use sequence-utilities, import: { partition };
   // from system
   use locators, import: { <url> };
   
   export
      process-markup;
end module;
