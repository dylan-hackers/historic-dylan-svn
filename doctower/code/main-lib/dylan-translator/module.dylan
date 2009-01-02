module: dylan-user

define module dylan-translator
   use common;
   use conditions;
   use dylan-parser, rename: { source-location => token-src-loc };
   use internal-rep;
   use configs;
   
   export topics-from-source;
end module;