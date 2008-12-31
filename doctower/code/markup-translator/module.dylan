module: dylan-user
synopsis: This module converts from tokens to intermediate markup (the DITA-style
          markup used internally).

define module markup-translator
   use common;
   use conditions;
   use markup-parser,
      rename: { content => token-content, index => token-index,
                text => token-text, topics => token-topics,
                source-location => token-src-loc };
   use internal-rep;
   use configs;
   
   export topics-from-markup;
end module;
