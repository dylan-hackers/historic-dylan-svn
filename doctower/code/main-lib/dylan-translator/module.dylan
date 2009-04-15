module: dylan-user

define module dylan-translator
   use common;
   use conditions;
   use dylan-parser,
      rename: { source-location => token-src-loc, local-name => token-local-name,
                import-name => token-import-name, definitions => token-definitions,
                source-text => token-text };
   use dylan-rep;
   use markup-rep;
   use configs;
   
   export topics-from-dylan;
end module;