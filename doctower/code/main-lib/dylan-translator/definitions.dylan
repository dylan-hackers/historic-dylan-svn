module: dylan-translator
synopsis: Code dealing with class, generic, etc. representations in general.


define method make-binding-from-tokens (tokens :: <sequence>)
=> (binding :: <binding>, definition :: <definition>)
   let expl/impl-defns = map(make-expl/impl-defn, tokens);
   let defn-class =
         case
            every?(rcurry(instance?, <explicit-class-defn>),
                   expl/impl-defns)
               => <class-defn>;
            every?(rcurry(instance?,
                          type-union(<explicit-generic-defn>, <implicit-generic-defn>)),
                   expl/impl-defns)
               => <generic-defn>;
            every?(rcurry(instance?, <explicit-function-defn>),
                   expl/impl-defns)
               => <function-defn>;
            every?(rcurry(instance?, <explicit-constant-defn>),
                   expl/impl-defns)
               => <constant-defn>;
            every?(rcurry(instance?, <explicit-variable-defn>),
                   expl/impl-defns)
               => <variable-defn>;
            every?(rcurry(instance?, <explicit-macro-defn>),
                   expl/impl-defns)
               => <macro-defn>;
            otherwise =>
               let locs = map(source-location, expl/impl-defns);
               conflicting-bindings-in-module(location: #f, name: tokens.first.api-name,
                     defn-locations: locs.item-string-list);
         end case;

   let (impl-defns, expl-defns) =
         partition(rcurry(instance?, <implicit-generic-defn>), expl/impl-defns);
   unless (expl-defns.size <= 1)
      let locs = map(source-location, expl-defns);
      conflicting-bindings-in-module(location: #f, name: tokens.first.api-name,
            defn-locations: locs.item-string-list)
   end unless;

   let definition = make(defn-class);
   definition.explicit-defn := ~expl-defns.empty? & expl-defns.first;
   unless (impl-defns.empty?)
      debug-assert(defn-class = <generic-defn>);
      definition.implicit-defns := as(<stretchy-vector>, impl-defns);
   end unless;
   
   let new-binding = make(<local-binding>, local-name: tokens.first.api-name,
                          definition: definition,
                          source-location: tokens.first.token-src-loc);

   values(new-binding, definition);
end method;


define method make-binding
   (library :: <library>, module :: <module>, binding-class :: subclass(<binding>),
    #rest keys, #key, #all-keys)
=> (binding :: <binding>)
   let new-binding = apply(make, binding-class, keys);
   check-no-binding(module, new-binding);
   module.bindings := add!(module.bindings, new-binding);
   new-binding
end method;


define method make-created-binding
   (library :: <library>, module :: <module>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>)
=> (binding :: <binding>)
   apply(make-binding, library, module, <local-binding>,
         definition:, #f, exported:, #t, keys);
end method;


define method make-stray-binding
   (library :: <library>, module :: <local-module>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         exported :: <boolean> = #f)
=> (binding :: <binding>)
   let binding-class =
         if (inferred-module?(library, module))
            <local-binding>
         else
            <imported-binding>
         end if;
   apply(make-binding, library, module, binding-class,
         import-name: local-name, used-module: #f, keys);
end method;


define method make-stray-binding
   (library :: <library>, module :: <imported-module>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         exported :: <boolean> = #f)
=> (binding :: <binding>)
   apply(make-binding, library, module, <local-binding>, keys);
end method;


define method make-imported-binding
   (library :: <library>, module :: <local-module>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         used-module :: <module>, import-name :: <string>,
         exported :: <boolean> = #f)
=> (binding :: <binding>)
   let binding-class =
         if (inferred-module?(library, module))
            <local-binding>
         else
            <imported-binding>
         end if;
   apply(make-binding, library, module, binding-class, keys);
end method;


define method make-imported-binding
   (library :: <library>, module :: <imported-module>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         exported :: <boolean> = #f)
=> (binding :: <binding>)
   apply(make-binding, library, module, <local-binding>, keys);
end method;


define method check-no-binding (module :: <module>, new-binding :: <binding>) => ()
   let existing = find-element(module.bindings,
                               rcurry(has-local-name?, new-binding.local-name));
   when (existing & existing ~= new-binding)
      let locs = vector(new-binding.source-location, existing.source-location);
      conflicting-bindings-in-module(location: module.source-location,
            name: new-binding.local-name, defn-locations: locs.item-string-list);
   end when;
end method;


//
// Placeholders for types of definitions
//


define method make-expl/impl-defn (token :: <class-definer-token>)
=> (defn :: <explicit-class-defn>)
   make(<explicit-class-defn>, source-location: token.token-src-loc);
end method;


define method make-expl/impl-defn (token :: <generic-definer-token>)
=> (defn :: <explicit-generic-defn>)
   make(<explicit-generic-defn>, source-location: token.token-src-loc);
end method;


define method make-expl/impl-defn (token :: <method-definer-token>)
=> (defn :: <implicit-generic-defn>)
   make(<implicit-generic-defn>, source-location: token.token-src-loc);
end method;


define method make-expl/impl-defn (token :: <function-definer-token>)
=> (defn :: <explicit-function-defn>)
   make(<explicit-function-defn>, source-location: token.token-src-loc);
end method;


define method make-expl/impl-defn (token :: <constant-definer-token>)
=> (defn :: <explicit-constant-defn>)
   make(<explicit-constant-defn>, source-location: token.token-src-loc);
end method;


define method make-expl/impl-defn (token :: <variable-definer-token>)
=> (defn :: <explicit-variable-defn>)
   make(<explicit-variable-defn>, source-location: token.token-src-loc);
end method;


define method make-expl/impl-defn (token :: <macro-definer-token>)
=> (defn :: <explicit-macro-defn>)
   make(<explicit-body-macro-defn>, source-location: token.token-src-loc);
end method;

