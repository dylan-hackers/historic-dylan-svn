module: dylan-user
synopsis: Code to translate Dylan parser tokens to higher-level API representations.
          See translator.dylan for entry point.

define module dylan-translator
   use common, rename: { binding-name => dyn-binding-name };
   use conditions;
   use dylan-parser,
      rename: { source-location => token-src-loc, local-name => token-local-name,
                import-name => token-import-name, slot-name => token-slot-name,
                slot-setter => token-slot-setter, definitions => token-definitions,
                source-text => token-text, <class-slot> => <parsed-class-slot>,
                <rest-value> => <parsed-rest-value> };
   use markup-parser, import: { <markup-content-token>, default-topic-content };
   use dylan-rep;
   use markup-rep,
      rename: { api-type => api-list-type, api-type-setter => api-list-type-setter};
   
   export apis-from-dylan, $object-type;
end module;
