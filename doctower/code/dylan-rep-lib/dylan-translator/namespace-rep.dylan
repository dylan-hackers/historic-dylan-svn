module: dylan-translator
synopsis: Common code dealing with library and module representations.


//
// Library and module annotations
//


/**
Library and module annotations track dependencies, but they track them by way of
<use-clause-token>s rather than simple names so that source locations are
available.
**/
define class <namespace-annotation> (<object>)
   slot annot-dependencies :: <sequence> /* of <use-clause-token> */ = #[];
end class;


//
// Checks
//


define method check-and-flag-library-exports
   (annotations :: <skip-list>, library :: <library>)
=> ()
   let lib-annot = annotations[library.local-name];
   check-clause-items(lib-annot.annot-token, <export-clause-token>, export-names,
         curry(key-exists?, lib-annot.annot-modules), 
         method (name :: <string>)
            lib-annot.annot-modules[name].annot-module.exported? := #t;
         end method,
         method (clause :: <token>, names :: <sequence>)
            let quoted-names = map(curry(format-to-string, "\"%s\""), names);
            no-definition-for-modules(location: clause.token-src-loc,
                                      names: quoted-names.item-string-list)
         end)
end method;


define method check-and-flag-module-exports
   (annotations :: <skip-list>, library :: <library>, module :: <module>)
=> ()
   let mod-annot = annotations[library.local-name].annot-modules[module.local-name];
   check-clause-items(mod-annot.annot-token, <export-clause-token>, export-names,
         method (name :: <string>)
            let annot = element(mod-annot.annot-bindings, name, default: #f);
            annot & ~annot.annot-source-defns.empty?
         end method,
         method (name :: <string>)
            mod-annot.annot-bindings[name].annot-binding.exported? := #t;
         end method,
         method (clause :: <token>, names :: <sequence>)
            let quoted-names = map(curry(format-to-string, "\"%s\""), names);
            no-definition-for-bindings(location: clause.token-src-loc,
                                       names: quoted-names.item-string-list)
         end)
end method;


define method check-and-flag-module-creates
   (annotations :: <skip-list>, library :: <library>, module :: <module>)
=> ()
   let mod-annot = annotations[library.local-name].annot-modules[module.local-name];
   check-clause-items(mod-annot.annot-token, <create-clause-token>, create-names,
         method (name :: <string>)
            let annot = element(mod-annot.annot-bindings, name, default: #f);
            annot & annot.annot-source-defns.empty?
         end method,
         method (name :: <string>)
            mod-annot.annot-bindings[name].annot-binding.exported? := #t;
         end method,
         method (clause :: <token>, names :: <sequence>)
            let existing-name = names.first;
            let existing-defns = mod-annot.annot-bindings[existing-name]
                  .annot-source-defns;
            let existing-locs = map(token-src-loc, add(existing-defns, clause));
            conflicting-bindings-in-module(location: clause.token-src-loc,
                  name: existing-name, defn-locations: existing-locs.item-string-list)
         end)
end method;


define method check-clause-items
   (token :: <definition-token>, clause-type :: <class>, name-list :: <function>,
    test-for-name :: <function>, action-for-name :: <function>, fail :: <function>)
=> ()
   let clauses = choose(rcurry(instance?, clause-type), token.namespace-clauses);
   let checked-names = #[];
   for (clause in clauses)
      let names = remove-duplicates(clause.name-list, test: case-insensitive-equal?);
      let new-names = difference(names, checked-names, test: case-insensitive-equal?);
      let test-results = map(test-for-name, new-names);
      let failed-names = choose-by(false?, test-results, new-names);
      unless (failed-names.empty?)
         fail(clause, failed-names)
      end unless;
      do(action-for-name, choose-by(true?, test-results, new-names));
      checked-names := concatenate!(checked-names, new-names);
   end for;
end method;


//
// Dependencies
//


define method order-by-dependency (annotations :: <skip-list>) => ()
   order-list-by-dependency(annotations);
   for (lib-annot in annotations)
      order-list-by-dependency(lib-annot.annot-modules)
   end for;
end method;


define method order-list-by-dependency (annotations :: <skip-list>) => ()

   // Get complete dependency list of every element.

   let status = make(<case-insensitive-skip-list>);

   local method full-dependencies (name :: <string>, loc :: <source-location>)
         => (full-dependencies :: <sequence>)
            select (element(status, name, default: #"not done"))
               #"not done" =>
                  let annot = element(annotations, name, default: #f);
                  if (annot)
                     status[name] := loc;
                     for (use-clause in annot.annot-dependencies.copy-sequence)
                        let used-annot = element(annotations, use-clause.use-name,
                                                 default: #f);
                        if (used-annot)
                           annot.annot-dependencies := concatenate!
                                 (annot.annot-dependencies,
                                  full-dependencies(use-clause.use-name,
                                                    use-clause.token-src-loc));
                        end if;
                     end for;
                     status[name] := #"done";
                     annot.annot-dependencies := remove-duplicates!
                           (annot.annot-dependencies, test: same-use-name?);
                  else
                     #[];
                  end if;
               #"done" =>
                  annotations[name].annot-dependencies;
               otherwise =>
                  let in-prog-loc = choose(rcurry(instance?, <source-location>),
                                           status.element-sequence);
                  in-prog-loc := add-new!(in-prog-loc, loc, test: \=);
                  circular-definition(location: in-prog-loc.first, name: name,
                        defn-locations: in-prog-loc.item-string-list);
            end select;
         end method;

   for (annot keyed-by name in annotations)
      let loc =
            case
               annot.annot-token => annot.annot-token.token-src-loc;
               otherwise => annot.annot-representation.source-location;
            end case;
      annot.annot-dependencies := full-dependencies(name, loc);
   end for;
   
   // Sort by number of dependencies.
   
   local method compare-dependency-size
            (annot1 :: <namespace-annotation>, annot2 :: <namespace-annotation>)
         => (annot1-less? :: <boolean>)
            let annot1-deps = annot1.annot-dependencies.size;
            let annot2-deps = annot2.annot-dependencies.size;
            annot1-deps < annot2-deps;
         end method;

   annotations.element-sequence :=
         sort(annotations.element-sequence, test: compare-dependency-size);
end method;


//
// Inferring and updating
//


define method infer-and-import-items
   (lib-annots :: <skip-list>,
    local-lib :: <library>, local-mod :: false-or(<module>),
    used-lib :: <library>, used-mod :: false-or(<module>),
    clause :: false-or(<use-clause-token>))
=> ()

   // Interpret use clause options.

   let import-options :: type-union(<sequence> /* of <renaming-token> */,
                                    singleton(#"all")) =
         (clause & clause.use-imports) | #"all";
   let rename-options :: <sequence> /* of <renaming-token> */ =
         (clause & clause.use-renamings) | #[];
   let exclude-options :: <sequence> /* of <string> */ =
         (clause & clause.use-exclusions) | #[];
   let export-options :: type-union(<sequence> /* of <string> */,
                                    singleton(#"all")) =
         if (clause)
            clause.use-exports | #[]
         else
            #"all"
         end if;
   let prefix-option :: false-or(<string>) =
         clause & clause.use-prefix;
   
   let import-and-rename-options :: <sequence> /* of <renaming-token> */ =
         concatenate(if (import-options = #"all") #[] else import-options end,
                     rename-options);
   
   let reason-location =
         if (clause)
            clause.token-src-loc
         else
            (local-mod | local-lib).source-location
         end if;

   // Utility functions.

   local method import-name-of-renaming?
            (name :: <string>, renaming :: <renaming-token>)
         => (match? :: <boolean>)
            case-insensitive-equal?(name, renaming.token-import-name)
         end method,
         
         method local-name-of-renaming?
            (name :: <string>, renaming :: <renaming-token>)
         => (match? :: <boolean>)
            case-insensitive-equal?(name,
                  renaming.token-local-name | renaming.token-import-name)
         end method,
         
         method local-name-of-renaming (renaming :: <renaming-token>)
         => (local-name :: <string>)
            if (renaming.token-local-name)
               renaming.token-local-name
            elseif (prefix-option)
               concatenate(prefix-option, renaming.token-import-name)
            else
               renaming.token-import-name
            end if;
         end method,
         
         method local-names-for-imported-name (import-name :: <string>)
         => (local-names :: <sequence> /* of <string> */)
            let renamings = choose(curry(import-name-of-renaming?, import-name),
                                   import-and-rename-options);
            case
               ~renamings.empty? => map(local-name-of-renaming, renamings);
               prefix-option => vector(concatenate(prefix-option, import-name));
               otherwise => vector(import-name);
            end case
         end method,
         
         method import-name-for-local-name (local-name :: <string>)
         => (import-name :: <string>)
            let renaming = find-element(import-and-rename-options,
                                        curry(local-name-of-renaming?, local-name),
                                        failure: #f);
            case
               renaming => renaming.token-import-name;
               prefix-option =>
                  let maybe-prefix = copy-sequence(local-name, end: prefix-option.size);
                  if (case-insensitive-equal?(maybe-prefix, prefix-option))
                     copy-sequence(local-name, start: prefix-option.size)
                  else
                     local-name
                  end if;
               otherwise => local-name;
            end case
         end method;

   // Get existing importable and imported names from used.
   
   let existing-items-in-used = choose(exported?,
         annot-contents(lib-annots, used-lib, used-mod).element-sequence);
   let existing-names-in-used = map(local-name, existing-items-in-used);
   let existing-non-excluded-names-in-used =
         difference(existing-names-in-used, exclude-options,
                    test: case-insensitive-equal?);
   let existing-imported-names-in-used =
         if (import-options = #"all")
            existing-non-excluded-names-in-used
         else
            let imported-names-in-used = map(token-import-name, import-options);
            intersection(existing-non-excluded-names-in-used, imported-names-in-used,
                         test: case-insensitive-equal?)
         end if;

   // Get existing imported items from local.
   
   let existing-imports-in-local = choose(
         method (item) => (imp?)
            imported-from?(local-lib, local-mod, item, used-lib, used-mod)
         end,
         annot-contents(lib-annots, local-lib, local-mod).element-sequence);
   let existing-import-names-in-local = map(local-name, existing-imports-in-local);

   // Get all known importable names from used.
   
   let explicit-names-in-used-from-imports =
         map(token-import-name, import-and-rename-options);
   let explicit-names-in-used-from-exports =
         if (export-options ~= #"all")
            map(import-name-for-local-name, export-options);
         else
            #[];
         end if;
   let expected-names-in-used-from-imports =
         map(import-name-for-local-name, existing-import-names-in-local);
   let expected-names-in-used = remove-duplicates
            (concatenate(existing-names-in-used, exclude-options,
                         explicit-names-in-used-from-imports,
                         explicit-names-in-used-from-exports,
                         expected-names-in-used-from-imports),
             test: case-insensitive-equal?);

   // Create stray importables for missing names in used.
   
   let missing-names-in-used =
         difference(expected-names-in-used, existing-names-in-used,
                    test: case-insensitive-equal?);
   do(curry(infer-in-used-namespace, lib-annots, used-lib, used-mod,
            source-location:, reason-location, exported:, #t, local-name:),
      missing-names-in-used);
   
   // Determine local name or names (if multiple import/rename options) of
   // each imported name.

   let existing-names-in-used = existing-imported-names-in-used;
   let expected-names-in-local :: <sequence> /* of <sequence> of <string> */
         = map(local-names-for-imported-name, existing-names-in-used);

   // Make items, or update previously inferred items.

   for (name-in-used in existing-names-in-used,
        names-in-local in expected-names-in-local)
      for (name-in-local in names-in-local)
         let exported = (export-options = #"all") |
               member?(name-in-local, export-options, test: case-insensitive-equal?);
         let used-item = annot-contents(lib-annots, used-lib, used-mod)[name-in-used];
         let local-item = element(annot-contents(lib-annots, local-lib, local-mod),
                                  name-in-local, default: #f);
         if (local-item)
            if (local-item.stray?)
               resolve-stray(local-item, used-lib, used-mod, used-item);
            end if;
            check-not-conflicting-items(local-lib, local-mod, local-item,
                                        used-lib, used-mod, used-item,
                                        reason-location);
            local-item.exported? := local-item.exported? | exported;
         else
            import-into-namespace(lib-annots, local-lib, local-mod,
                                  used-lib, used-mod, used-item,
                                  source-location: reason-location,
                                  local-name: name-in-local,
                                  exported: exported)
         end if;
      end for;
   end for;
   
   // Note unknown exports from used library.
   
   when (import-options = #"all" & export-options = #"all" &
         inferred-namespace?(used-lib, used-mod))
      note-unknown-reexport-source(local-lib, local-mod, used-lib, used-mod);
   end when;
end method;


define method annot-contents
   (lib-annots :: <skip-list>, lib :: <library>, mod == #f)
=> (contents :: <skip-list>)
   map(annot-module, lib-annots[lib.local-name].annot-modules);
end method;


define method annot-contents
   (lib-annots :: <skip-list>, lib :: <library>, mod :: <module>)
=> (contents :: <skip-list>)
   map(annot-binding,
       lib-annots[lib.local-name].annot-modules[mod.local-name].annot-bindings);
end method;


/**
Synopsis: Returns #t if item is imported from used, whether it actually exists
in used or not.
**/
define method imported-from?
   (local-lib :: <library>, local-mod :: false-or(<module>),
    local-item :: type-union(<module>, <binding>),
    used-lib :: <library>, used-mod :: false-or(<module>))
=> (imported? :: <boolean>)
   #f
end method;


define method imported-from?
   (local-lib :: <library>, local-dummy == #f, local-mod :: <imported-module>,
    used-lib :: <library>, used-dummy == #f)
=> (imported? :: <boolean>)
   ~local-mod.stray? & local-mod.used-library = used-lib
end method;


define method imported-from?
   (local-lib :: <library>, local-mod :: <module>, local-bind :: <imported-binding>,
    used-lib :: <library>, used-mod :: <module>)
=> (imported? :: <boolean>)
   ~local-bind.stray? & local-bind.used-module = used-mod
end method;


/** All bindings of an <imported-module> are effectively imported. **/
define method imported-from?
   (local-lib :: <library>, local-mod :: <imported-module>, local-bind :: <binding>,
    used-lib :: <library>, used-mod :: <module>)
=> (imported? :: <boolean>)
   #t
end method;


define method infer-in-used-namespace
   (lib-annots :: <skip-list>, used-lib :: <library>, used-mod == #f,
    #rest keys, #key source-location, exported, local-name: name)
=> (new-mod :: <module>, new-annot :: <module-annot>)
   // log("  Infer module %s in used library %s", name, used-lib.local-name);
   apply(make-stray-module, lib-annots, used-lib, keys)
end method;


define method infer-in-used-namespace
   (lib-annots :: <skip-list>, used-lib :: <library>, used-mod :: <module>,
    #rest keys, #key source-location, exported, local-name: name)
=> (new-bind :: <binding>, new-annot :: <binding-annot>)
   // log("  Infer binding %s in %s:%s",
   //       name, used-lib.local-name, used-mod.local-name);
   apply(make-stray-binding, lib-annots, used-lib, used-mod, keys)
end method;


define method resolve-stray
   (local-mod :: <imported-module>, used-lib :: <library>, used-dummy == #f,
    used-mod :: <module>)
=> (local-mod :: <module>)
   // log("  Resolve stray module %s to %s:%s",
   //       local-mod.local-name, used-lib.local-name, used-mod.local-name);
   local-mod.used-library := used-lib;
   local-mod.import-name := used-mod.local-name;
   local-mod;
end method;


define method resolve-stray
   (local-bind :: <imported-binding>, used-lib :: <library>, used-mod :: <module>,
    used-bind :: <binding>)
=> (local-bind :: <binding>)
   // log("  Resolve stray binding %s to %s:%s:%s",
   //       local-bind.local-name, used-lib.local-name, used-mod.local-name,
   //       used-bind.local-name);
   local-bind.used-module := used-mod;
   local-bind.import-name := used-bind.local-name;
   local-bind;
end method;


define method import-into-namespace
   (lib-annots :: <skip-list>, local-lib :: <library>, local-dummy == #f,
    used-lib :: <library>, used-dummy == #f, used-mod :: <module>,
    #rest keys, #key source-location, exported, local-name: name)
=> (new-mod :: <module>, new-annot :: <module-annot>)
   // log("  Create module %s from %s:%s",
   //       name, used-lib.local-name, used-mod.local-name);
   apply(make-imported-module, lib-annots, local-lib,
         used-library:, used-lib, import-name:, used-mod.local-name,
         keys)
end method;


define method import-into-namespace
   (lib-annots :: <skip-list>, local-lib :: <library>, local-mod :: <module>,
    used-lib :: <library>, used-mod :: <module>, used-bind :: <binding>,
    #rest keys, #key source-location, exported, local-name: name)
=> (new-bind :: <binding>, new-annot :: <binding-annot>)
   // log("  Create binding %s from %s:%s:%s",
   //       name, used-lib.local-name, used-mod.local-name, used-bind.local-name);
   apply(make-imported-binding, lib-annots, local-lib, local-mod,
         used-module:, used-mod, import-name:, used-bind.local-name,
         keys)
end method;


define method inferred-namespace? (lib :: <library>, mod == #f)
=> (inferred? :: <boolean>)
   inferred-library?(lib)
end method;


define method inferred-namespace? (lib :: <library>, mod :: <module>)
=> (inferred? :: <boolean>)
   inferred-module?(lib, mod)
end method;


define method check-not-conflicting-items
   (local-lib :: <library>, local-dummy == #f, local-mod :: <module>,
    used-lib :: <library>, used-dummy == #f, used-mod :: <module>,
    location :: <source-location>)
=> ()
   unless (same-as-used-module?(local-lib, local-mod, used-lib, used-mod))
      let locs = vector(local-mod.source-location, used-mod.source-location);
      conflicting-modules-in-library(location: location, name: local-mod.local-name,
                                     defn-locations: locs.item-string-list);
   end unless;
end method;


define method check-not-conflicting-items
   (local-lib :: <library>, local-mod :: <module>, local-bind :: <binding>,
    used-lib :: <library>, used-mod :: <module>, used-bind :: <binding>,
    location :: <source-location>)
=> ()
   unless (same-as-used-binding?(local-lib, local-mod, local-bind,
                                 used-lib, used-mod, used-bind))
      let locs = vector(local-bind.source-location, used-bind.source-location);
      conflicting-bindings-in-module(location: location, name: local-bind.local-name,
                                     defn-locations: locs.item-string-list);
   end unless;
end method;


define method note-unknown-reexport-source
   (local-lib :: <library>, local-mod == #f, used-lib :: <library>, used-mod == #f)
=> ()
   local-lib.unknown-reexport-sources :=
         add-new!(local-lib.unknown-reexport-sources, used-lib, test: \=)
end method;


define method note-unknown-reexport-source
   (local-lib :: <library>, local-mod :: <module>,
    used-lib :: <library>, used-mod :: <module>)
=> ()
   local-mod.unknown-reexport-sources :=
         add-new!(local-mod.unknown-reexport-sources, used-mod, test: \=)
end method;
