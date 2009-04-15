module: dylan-translator
synopsis: Code dealing with class, generic, etc. representations in general.


define class <binding-annot> (<object>)
   slot annot-binding :: <binding>, required-init-keyword: #"binding";
   slot annot-tokens :: <sequence> /* of <definition-token> */ = #[];
end class;


/**
A local binding in a local module is created from a defining macro that is not
a method or generic definer. It cannot be the same as any other binding.
**/
define method same-as-used-binding?
   (local-lib :: <library>, local-mod :: <local-module>, local-bind :: <local-binding>,
    used-lib :: <library>, used-mod :: <module>, used-bind :: <binding>)
=> (same? :: <boolean>)
   local-bind == used-bind
end method;


/**
An imported binding is the same as a binding in another module if:
   - It is imported from that binding in that module. This is true even if the
     binding is made by a method or generic definer; in this case, the methods
     of the generic will be merged.
   - Both bindings have the same name. Ideally, we would want to ensure the
     chain of imports for each binding lead to a common ancestor, but we will
     not have enough information to determine this. We hope it will all work
     out.
**/
define method same-as-used-binding?
   (local-lib :: <library>, local-mod :: <local-module>, local-bind :: <imported-binding>,
    used-lib :: <library>, used-mod :: <module>, used-bind :: <binding>)
=> (same? :: <boolean>)
   case
      local-bind == used-bind => #t;
      local-mod == used-mod => local-bind = used-bind;
      local-bind.stray? =>
         case-insensitive-equal?(local-bind.local-name, used-bind.local-name);
      otherwise =>
         case-insensitive-equal?(local-bind.import-name, used-bind.local-name);
   end case;
end method;


/**
In an imported module, all bindings are the same as corresponding bindings in
the used module. If the local module is stray, any other module with the same
local name is assumed to be the used module.
**/
define method same-as-used-binding?
   (local-lib :: <library>, local-mod :: <imported-module>, local-bind :: <local-binding>,
    used-lib :: <library>, used-mod :: <module>, used-bind :: <binding>)
=> (same? :: <boolean>)
   case
      local-bind == used-bind => #t;
      local-mod == used-mod => local-bind = used-bind;
      local-mod.stray? =>
         case-insensitive-equal?(local-mod.local-name, used-mod.local-name)
               & case-insensitive-equal?(local-bind.local-name, used-bind.local-name);
      otherwise =>
         local-mod.used-library = used-lib
               & case-insensitive-equal?(local-mod.import-name, used-mod.local-name)
               & case-insensitive-equal?(local-bind.local-name, used-bind.local-name);
   end case
end method;


//
// Binding creation
//


define method make-created-bindings
   (annotations :: <skip-list>, library :: <library>, module :: <module>)
=> ()
   make-bindings-from-clauses
         (annotations, library, module, <create-clause-token>, create-names)
end method;


define method make-exported-bindings
   (annotations :: <skip-list>, library :: <library>, module :: <module>)
=> ()
   make-bindings-from-clauses
         (annotations, library, module, <export-clause-token>, export-names)
end method;


define method make-bindings-from-clauses
   (annotations :: <skip-list>, library :: <library>, module :: <module>,
    clause-type :: <class>, name-list :: <function>)
=> ()
   let mod-annot = annotations[library.local-name].annot-modules[module.local-name];
   let token :: <module-definer-token> = mod-annot.annot-token;
   let clauses = choose(rcurry(instance?, clause-type), token.namespace-clauses);
   let resolved-names = #[];
   for (clause in clauses)
      let names = remove-duplicates(clause.name-list, test: case-insensitive-equal?);
      let new-names = difference(names, resolved-names, test: case-insensitive-equal?);
      do(curry(make-created/exported-binding, annotations, library, module,
               source-location:, clause.token-src-loc, local-name:),
         new-names);
      resolved-names := concatenate!(resolved-names, new-names);
   end for;
end method;


define method make-defined-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <module>,
    tokens :: <sequence> /* of <definition-token> */)
=> (binding :: <binding>, annotation :: <binding-annot>)
   
   // Make implicit or explicit definitions and potential binding names.

   let binding-name = tokens.first.api-name;
   let expl/impl-defns = make(<vector>, size: tokens.size);
   let source-names = make(<stretchy-vector>);
   for (token in tokens, i from 0)
      let (defn, names) = make-expl/impl-defn(token);
      expl/impl-defns[i] := defn;
      source-names := concatenate!(source-names, names);
   end for;
   
   // Check for consistency.
   
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
               conflicting-bindings-in-module(location: module.source-location,
                     name: binding-name, defn-locations: locs.item-string-list);
         end case;

   let (impl-defns, expl-defns) =
         partition(rcurry(instance?, <implicit-generic-defn>), expl/impl-defns);

   unless (expl-defns.size <= 1)
      let locs = map(source-location, expl-defns);
      conflicting-bindings-in-module(location: module.source-location,
            name: binding-name, defn-locations: locs.item-string-list);
   end unless;
   
   // Make new definition.
   
   let new-defn = make(defn-class);
   new-defn.explicit-defn := ~expl-defns.empty? & expl-defns.first;
   when (instance?(new-defn, <generic-defn>))
      new-defn.implicit-defns := as(<stretchy-vector>, impl-defns);
   end when;

   // Create new or get existing binding and add definition.

   let bind-annots = lib-annots[library.local-name]
         .annot-modules[module.local-name].annot-bindings;
   let annot = element(bind-annots, binding-name, default: #f);
   let (binding, annot) =
         if (annot)
            // This will be the case if the module declared the binding as exported.
            let binding = annot.annot-binding;
            if (binding.definition)
               add-definition!(binding.definition, new-defn)
            else
               binding.definition := new-defn
            end if;
            values(binding, annot);
         else
            let loc = (~expl-defns.empty? & expl-defns.first.source-location)
                  | impl-defns.first.source-location;
            make-local-binding(lib-annots, library, module,
                  local-name: binding-name, definition: new-defn,
                  source-location: loc, exported: #f);
         end if;
   annot.annot-tokens := tokens;
   
   // Make potential binding names.
   
   let existing-names = map(local-name, module.bindings);
   source-names := remove-duplicates!(source-names, test: same-source-name?);
   source-names := difference(source-names, existing-names, test: has-source-name?);
   do(method (name :: <source-name>)
         make-stray-binding(lib-annots, library, module, local-name: name.source-name,
                            source-location: name.source-location, exported: #f)
      end, source-names);

   values(binding, annot)
end method;


define method make-annotated-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <module>,
    binding-class :: subclass(<binding>), #rest keys,
    #key local-name: name :: <string>, #all-keys)
=> (binding :: <binding>, annotation :: <binding-annot>)
   let new-binding = apply(make, binding-class, keys);
   let bind-annots = lib-annots[library.local-name]
         .annot-modules[module.local-name].annot-bindings;
   check-no-annotated-binding(bind-annots, module, new-binding);
   let new-annot = make(<binding-annot>, binding: new-binding);
   bind-annots[name] := new-annot;
   module.bindings := add!(module.bindings, new-binding);
   values(new-binding, new-annot)
end method;


define method make-created/exported-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <module>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>)
=> (binding :: <binding>, annotation :: <binding-annot>)
   apply(make-annotated-binding, lib-annots, library, module, <local-binding>,
         definition:, #f, exported:, #t, keys);
end method;


define method make-local-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <local-module>,
    #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         exported :: <boolean> = #f, definition :: <definition>)
=> (binding :: <binding>, annotation :: <binding-annot>)
   let binding-class =
         if (instance?(definition, <generic-defn>))
            <imported-binding>
         else
            <local-binding>
         end if;
   apply(make-annotated-binding, lib-annots, library, module, binding-class,
         import-name:, local-name, used-module:, #f, keys);
end method;


define method make-stray-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <local-module>,
    #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         exported :: <boolean> = #f, definition :: false-or(<definition>) = #f)
=> (binding :: <binding>, annotation :: <binding-annot>)
   let binding-class =
         if (inferred-module?(library, module))
            <local-binding>
         else
            <imported-binding>
         end if;
   apply(make-annotated-binding, lib-annots, library, module, binding-class,
         import-name:, local-name, used-module:, #f, keys);
end method;


define method make-stray-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <imported-module>,
    #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         exported :: <boolean> = #f, definition :: false-or(<definition>) = #f)
=> (binding :: <binding>, annotation :: <binding-annot>)
   apply(make-annotated-binding, lib-annots, library, module, <local-binding>, keys);
end method;


define method make-imported-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <local-module>,
    #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         used-module :: <module>, import-name :: <string>,
         exported :: <boolean> = #f, definition :: false-or(<definition>) = #f)
=> (binding :: <binding>, annotation :: <binding-annot>)
   let binding-class =
         if (inferred-module?(library, module))
            <local-binding>
         else
            <imported-binding>
         end if;
   apply(make-annotated-binding, lib-annots, library, module, binding-class, keys);
end method;


define method make-imported-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <imported-module>,
    #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         used-module :: <module>, import-name :: <string>,
         exported :: <boolean> = #f, definition :: false-or(<definition>) = #f)
=> (binding :: <binding>, annotation :: <binding-annot>)
   apply(make-annotated-binding, lib-annots, library, module, <local-binding>,
         keys);
end method;


define method check-no-annotated-binding
   (bind-annots :: <skip-list>, module :: <module>, new-binding :: <binding>)
=> ()
   let existing = element(bind-annots, new-binding.local-name, default: #f);
   when (existing)
      let locs = vector(new-binding.source-location,
                        existing.annot-binding.source-location);
      conflicting-bindings-in-module(location: module.source-location,
            name: new-binding.local-name, defn-locations: locs.item-string-list);
   end when;
end method;


//
// Inferences and imports
//


define method infer-and-import-for-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <local-module>,
    binding :: <local-binding>)
=> ()
   // A local binding in a local module has nowhere to infer into or import from;
   // do nothing.
end method;


define method infer-and-import-for-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <local-module>,
    binding :: <imported-binding>)
=> ()
   unless (binding.stray?)
      // log("Promote/import definition of %s:%s:%s (imported binding)",
      //       library.local-name, module.local-name, binding.local-name);
      let used-mod = binding.used-module;
      let used-bind = lib-annots[library.local-name]
            .annot-modules[used-mod.local-name]
            .annot-bindings[binding.import-name]
            .annot-binding;
      if (used-bind.definition)
         used-bind.definition :=
               add-definition!(used-bind.definition, binding.definition);
         binding.definition := used-bind.definition;
      else
         used-bind.definition := binding.definition;
      end if;
   end unless;
end method;


define method infer-and-import-for-binding
   (lib-annots :: <skip-list>, library :: <library>, module :: <imported-module>,
    binding :: <binding>)
=> ()
   unless (module.stray?)
      // log("Promote/import definition of %s:%s:%s (imported module)",
      //       library.local-name, module.local-name, binding.local-name);
      let used-lib = module.used-library;
      let used-mod-name = module.import-name;
      let used-mod-annot = lib-annots[used-lib.local-name].annot-modules[used-mod-name];
      let used-mod = used-mod-annot.annot-module;
      let used-bind = used-mod-annot.annot-bindings[binding.local-name].annot-binding;
      if (used-bind.definition)
         used-bind.definition :=
               add-definition!(used-bind.definition, binding.definition);
         binding.definition := used-bind.definition;
      else
         used-bind.definition := binding.definition;
      end if;
   end unless;
end method;

