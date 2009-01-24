module: dylan-user

define module dylan-translator
   use common;
   use conditions;
   use dylan-parser,
      rename: { source-location => token-src-loc, local-name => token-local-name,
                import-name => token-import-name };
   use api-rep;
   use internal-rep;
   use configs;
   
   export organize-apis;
end module;