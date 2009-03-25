module: dylan-translator
synopsis: Code dealing specifically with module representations.


define class <module-annot> (<namespace-annotation>)
   slot annot-module :: <module>, required-init-keyword: #"module";
   slot annot-token :: false-or(<module-definer-token>) = #f,
         init-keyword: #"token";
   slot annot-definitions :: <sequence> /* of class, etc., definer tokens */ = #[];
   // TODO: Add annot-bindings as quick reference to bindings in the module,
   // rather than calling find-element all the time.
end class;


define method annot-representation (annot :: <module-annot>)
=> (mod :: <module>)
   annot.annot-module
end method;


define method inferred-module? (lib :: <library>, mod :: <imported-module>)
=> (inferred? :: <boolean>)
   mod.stray? | mod.used-library.inferred-library? | next-method()
end method;


define method inferred-module? (lib :: <library>, mod :: <module>)
   lib.inferred-library?
end method;


//
// Module creation
//


define method make-annotated-module
   (mod-annots :: <skip-list>, token :: false-or(<module-definer-token>),
    library :: <library>, module-class :: subclass(<module>), #rest keys,
    #key local-name: name :: <string>, #all-keys)
=> (module :: <module>, annotation :: <module-annot>)
   let new-module = apply(make, module-class, keys);
   check-no-annotated-module(mod-annots, library, new-module);
   let new-annot = make(<module-annot>, module: new-module, token: token);
   mod-annots[name] := new-annot;
   library.modules := add!(library.modules, new-module);
   values(new-module, new-annot);
end method;


define method make-defined-module
   (mod-annots :: <skip-list>, token :: <module-definer-token>, library :: <library>,
    #rest keys, #key local-name :: <string>, source-location :: <source-location>)
=> (module :: <module>, annotation :: <module-annot>)
   apply(make-annotated-module, mod-annots, token, library, <local-module>, keys);
end method;


define method make-stray-module
   (mod-annots :: <skip-list>, library :: <library>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         exported :: <boolean> = #f)
=> (module :: <module>, annotation :: <module-annot>)
   let module-class =
         if (library.inferred-library?)
            <local-module>
         else
            <imported-module>
         end if;
   apply(make-annotated-module, mod-annots, #f, library, module-class,
         import-name:, local-name, used-library:, #f, keys);
end method;


define method make-imported-module
   (mod-annots :: <skip-list>, library :: <library>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         used-library :: <library>, import-name :: <string>,
         exported :: <boolean> = #f)
=> (module :: <module>, annotation :: <module-annot>)
   let module-class =
         if (library.inferred-library?)
            <local-module>
         else
            <imported-module>
         end if;
   apply(make-annotated-module, mod-annots, #f, library, module-class, keys);
end method;


define method check-no-annotated-module
   (mod-annots :: <skip-list>, library :: <library>, new-module :: <module>)
=> ()
   let existing = element(mod-annots, new-module.local-name, default: #f);
   when (existing)
      let locs = vector(new-module.source-location, existing.annot-module.source-location);
      conflicting-modules-in-library(location: library.source-location,
            name: new-module.local-name, defn-locations: locs.item-string-list);
   end when;
end method;


//
// Inferences and imports
//


define method infer-and-import-for-module
   (mod-annots :: <skip-list>, library :: <library>, module :: <module>)
=> ()
   assert(inferred-module?(library, module))
   // Inferred modules have no imports and no place to infer in; do nothing.
end method;


/**
Synopsis: Create stray bindings in a module of another library and import
bindings that are known to exist.

Imported bindings will be instances of <local-binding> in this module.
**/
define method infer-and-import-for-module
   (mod-annots :: <skip-list>, library :: <library>, module :: <imported-module>)
=> ()
   local method same-binding? (bind1 :: <binding>, bind2 :: <binding>)
            has-local-name?(bind1, bind2.local-name)
         end method;
   
   unless (module.stray?)
      let used-mod-name = module.import-name;
      let used-mod = find-element
            (module.used-library.modules, rcurry(has-local-name?, used-mod-name));
      let used-bindings = choose(exported?, used-mod.bindings);

      // Infer bindings in other library and module.

      let missing-used-bindings = difference(module.bindings, used-bindings,
                                             test: same-binding?);
                  
      for (missing-binding :: <binding> in missing-used-bindings)
         make-stray-binding(module.used-library, used-mod, exported: #t,
                            local-name: missing-binding.local-name,
                            source-location: missing-binding.source-location)
      end for;
   
      // Import bindings from other library and module.

      let new-used-bindings = difference(used-bindings, module.bindings,
                                         test: same-binding?);

      for (used-binding :: <binding> in new-used-bindings)
         make-imported-binding(library, module, exported: #t,
                               local-name: used-binding.local-name,
                               definition: used-binding.definition,
                               source-location: used-binding.source-location)
      end for;
   end unless;
end method;


/**
Synopsis: Create stray bindings in another module of this library and import
bindings that are known to exist.
**/
define method infer-and-import-for-module
   (mod-annots :: <skip-list>, library :: <known-library>, module :: <local-module>)
=> ()
   let annot = mod-annots[module.local-name];
   let use-clauses = choose(rcurry(instance?, <use-clause-token>),
                            annot.annot-token.namespace-clauses);
   do(curry(infer-and-import-module-clause, mod-annots, annot, library),
      use-clauses)
end method;


define method infer-and-import-module-clause
   (mod-annots :: <skip-list>, annot :: <module-annot>, library :: <library>,
    clause :: <use-clause-token>)
=> ()
   let module = annot.annot-module;

   // Create used module if it isn't already defined.
   let used-mod-name = clause.use-name;
   let (used-mod, used-mod-annot) =
         if (key-exists?(mod-annots, used-mod-name))
            values(mod-annots[used-mod-name].annot-module,
                   mod-annots[used-mod-name])
         else
            make-stray-module(mod-annots, library, local-name: used-mod-name,
                  exported: #f, source-location: clause.token-src-loc);
         end if;
   
   // Process use clause.
   infer-and-import-clause(clause, used-mod.bindings,

         // Make stray binding in used module.
         method (name :: <string>) => ()
            make-stray-binding(library, used-mod, local-name: name,
               exported: #t, source-location: clause.token-src-loc)
         end method,
         
         // Make or update binding in this module.
         method (local-name :: <string>, import-name :: <string>, export :: <boolean>)
         => ()
            let existing-binding = find-element
                  (module.bindings, rcurry(has-local-name?, local-name));
            case
               existing-binding & existing-binding.stray? =>
                  existing-binding.used-module := used-mod;
                  existing-binding.import-name := import-name;
                  existing-binding.exported? := existing-binding.exported? | export;
               otherwise =>
                  make-imported-binding(library, module,
                        local-name: local-name, import-name: import-name,
                        used-module: used-mod, exported: export,
                        source-location: clause.token-src-loc);
            end case;
         end method,
         
         // Note unknown reexport source.
         method () => ()
            if (inferred-module?(library, used-mod))
               module.unknown-reexport-sources := add-new!
                     (module.unknown-reexport-sources, used-mod, test: \=)
            end if;
         end method);
end method;
   

//
// Import propogation and definitions
//


define method define-and-import-all-for-module
   (mod-annots :: <skip-list>, library :: <library>, module :: <module>)
=> ()
   assert(inferred-module?(library, module))
   // Inferred modules have no imports; do nothing.
end method;


/**
Synopsis: Create bindings imported from a module from another library.

These bindings will be instances of <local-binding> in this module.
**/
define method define-and-import-all-for-module
   (mod-annots :: <skip-list>, library :: <library>, module :: <imported-module>)
=> ()
   local method same-binding? (bind1 :: <binding>, bind2 :: <binding>)
            has-local-name?(bind1, bind2.local-name)
         end method;
   
   unless (module.stray?)
      let used-mod-name = module.import-name;
      let used-mod = find-element
            (module.used-library.modules, rcurry(has-local-name?, used-mod-name));
      let used-bindings = choose(exported?, used-mod.bindings);

      // Import bindings from other library and module.

      let new-used-bindings = difference(used-bindings, module.bindings,
                                         test: same-binding?);

      for (used-binding :: <binding> in new-used-bindings)
         make-imported-binding(library, module, exported: #t,
                               local-name: used-binding.local-name,
                               definition: used-binding.definition,
                               source-location: used-binding.source-location)
      end for;
   end unless;
end method;


/**
Synopsis: Create bindings imported from other modules from this library and add
local definitions and bindings.
**/
define method define-and-import-all-for-module
   (mod-annots :: <skip-list>, library :: <known-library>, module :: <local-module>)
=> ()
   let annot = mod-annots[module.local-name];
   let token = annot.annot-token;
   let use-clauses = choose(rcurry(instance?, <use-clause-token>),
                            token.namespace-clauses);
   do(curry(import-all-module-clause, mod-annots, annot, library), use-clauses);
   
   let defn-tokens = group-elements(annot.annot-definitions, test: same-api-name?);
   for (group in defn-tokens)
      make-definition-from-tokens(group, library, module);
   end for;
end method;


define method import-all-module-clause
   (mod-annots :: <skip-list>, annot :: <module-annot>, library :: <library>,
    clause :: <use-clause-token>)
=> ()
   let module = annot.annot-module;
   let used-mod = mod-annots[clause.use-name].annot-module;
   infer-and-import-clause(clause, used-mod.bindings,

         // Make stray binding in used module.
         always(#f),
         
         // Make or update binding in this module.
         method (local-name :: <string>, import-name :: <string>, export :: <boolean>)
         => ()
            let existing-binding = find-element
                  (module.bindings, rcurry(has-local-name?, local-name));
            let used-binding = find-element
                  (used-mod.bindings, rcurry(has-local-name?, import-name));
            case
               existing-binding & existing-binding.stray? =>
                  existing-binding.used-module := used-mod;
                  existing-binding.import-name := import-name;
                  existing-binding.exported? := existing-binding.exported? | export;
                  existing-binding.definition := used-binding & used-binding.definition;
               ~existing-binding =>
                  make-imported-binding(library, module,
                        local-name: local-name, import-name: import-name,
                        used-module: used-mod, exported: export,
                        definition: used-binding & used-binding.definition,
                        source-location: clause.token-src-loc);
            end case;
         end method,
         
         // Note unknown reexport source.
         always(#f));
end method;


//
// Export checks
//


define method check-exports-not-imported (module :: <module>, token == #f)
=> ()
   // Do nothing if we don't have an export clause.
end method;


define method check-exports-not-imported
   (module :: <module>, token :: <module-definer-token>)
=> ()
   let export-clauses =
         choose(rcurry(instance?, <export-clause-token>), token.namespace-clauses);
   for (clause in export-clauses)
      for (name in clause.export-names)
         let found-bind = find-element(module.bindings, rcurry(has-local-name?, name));
         when (instance?(found-bind, <imported-binding>) & ~found-bind.stray?)
            let locs = vector(found-bind.source-location, clause.token-src-loc);
            conflicting-bindings-in-module(location: module.source-location,
                  name: name, defn-locations: locs.item-string-list);
         end when;
      end for;
   end for;
end method;


