module: dylan-translator
synopsis: Code dealing specifically with module representations.


define class <module-annot> (<namespace-annotation>)
   slot annot-module :: <module>, required-init-keyword: #"module";
   slot annot-token :: false-or(<module-definer-token>) = #f,
         init-keyword: #"token";
   slot annot-bindings = make(<case-insensitive-skip-list>, size: 150);
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


/**
Unknown libraries don't use modules from other libraries, including themselves,
so a module in an unknown library cannot be the same as any other module.
**/
define method same-as-used-module?
   (local-lib :: <unknown-library>, local-mod :: <module>,
    used-lib :: <library>, used-mod :: <module>)
=> (same? :: <boolean>)
   local-mod == used-mod
end method;


/**
A local module in a known library is created from a module-definer and cannot be
the same as any other module.
**/
define method same-as-used-module?
   (local-lib :: <known-library>, local-mod :: <local-module>,
    used-lib :: <library>, used-mod :: <module>)
=> (same? :: <boolean>)
   local-mod == used-mod
end method;


/**
An imported module is equal to a module in another library if it is imported
from that module in that library. Strays are assumed to be equal to any other
module with the same local name.
**/
define method same-as-used-module?
   (local-lib :: <known-library>, local-mod :: <imported-module>,
    used-lib :: <library>, used-mod :: <module>)
=> (same? :: <boolean>)
   case
      local-mod == used-mod => #t;
      local-lib == used-lib => local-mod = used-mod;
      local-mod.stray? =>
         case-insensitive-equal?(local-mod.local-name, used-mod.local-name);
      otherwise =>
         local-mod.used-library = used-lib
               & case-insensitive-equal?(local-mod.import-name, used-mod.local-name);
   end case
end method;


//
// Module creation
//


define method make-annotated-module
   (lib-annots :: <skip-list>, token :: false-or(<module-definer-token>),
    library :: <library>, module-class :: subclass(<module>), #rest keys,
    #key local-name: name :: <string>, #all-keys)
=> (module :: <module>, annotation :: <module-annot>)
   let mod-annots = lib-annots[library.local-name].annot-modules;
   let new-module = apply(make, module-class, keys);
   check-no-annotated-module(mod-annots, library, new-module);
   let new-annot = make(<module-annot>, module: new-module, token: token);
   mod-annots[name] := new-annot;
   library.modules := add!(library.modules, new-module);
   values(new-module, new-annot);
end method;


define method make-defined-module
   (lib-annots :: <skip-list>, token :: <module-definer-token>, library :: <library>,
    #rest keys, #key local-name :: <string>, source-location :: <source-location>)
=> (module :: <module>, annotation :: <module-annot>)
   let (module, annotation) = apply
         (make-annotated-module, lib-annots, token, library, <local-module>,
          markup: token.scoped-docs, keys);
   make-exported-bindings(lib-annots, library, module);
   make-created-bindings(lib-annots, library, module);
   values(module, annotation);
end method;


define method make-stray-module
   (lib-annots :: <skip-list>, library :: <library>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>,
         exported :: <boolean> = #f)
=> (module :: <module>, annotation :: <module-annot>)
   let module-class =
         if (library.inferred-library?)
            <local-module>
         else
            <imported-module>
         end if;
   apply(make-annotated-module, lib-annots, #f, library, module-class,
         import-name:, local-name, used-library:, #f, keys);
end method;


define method make-imported-module
   (lib-annots :: <skip-list>, library :: <library>, #rest keys,
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
   apply(make-annotated-module, lib-annots, #f, library, module-class, keys);
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
   (lib-annots :: <skip-list>, library :: <library>, module :: <module>)
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
   (lib-annots :: <skip-list>, library :: <library>, module :: <imported-module>)
=> ()
   unless (module.stray?)
      // log("Promote/import bindings in %s:%s (imported module)",
      //       library.local-name, module.local-name);
      let used-lib = module.used-library;
      let used-mod-name = module.import-name;
      let used-lib-annot = lib-annots[used-lib.local-name];
      let used-mod = used-lib-annot.annot-modules[used-mod-name].annot-module;
      infer-and-import-items(lib-annots, library, module, used-lib, used-mod, #f)
   end unless;
end method;


/**
Synopsis: Create stray bindings in another module of this library and import
bindings that are known to exist.
**/
define method infer-and-import-for-module
   (lib-annots :: <skip-list>, library :: <known-library>, module :: <local-module>)
=> ()
   // Annot.annot-dependencies also holds use clauses, but includes use clauses
   // gathered from other modules. We just want this module's, so reference
   // the module definition token directly.
   // log("Promote/import bindings in %s:%s", library.local-name, module.local-name);
   let mod-annots = lib-annots[library.local-name].annot-modules;
   let annot = mod-annots[module.local-name];
   let use-clauses = choose(rcurry(instance?, <use-clause-token>),
                            annot.annot-token.namespace-clauses);
   do(curry(infer-and-import-module-clause, lib-annots, mod-annots, annot,
            library, module),
      use-clauses)
end method;


define method infer-and-import-module-clause
   (lib-annots :: <skip-list>, mod-annots :: <skip-list>, mod-annot :: <module-annot>,
    library :: <library>, module :: <module>, clause :: <use-clause-token>)
=> ()
   let used-mod-name = clause.use-name;
   let (used-mod, used-mod-annot) =
         if (key-exists?(mod-annots, used-mod-name))
            values(mod-annots[used-mod-name].annot-module,
                   mod-annots[used-mod-name])
         else
            // log("  Infer used module %s", used-mod-name);
            make-stray-module(lib-annots, library, local-name: used-mod-name,
                  exported: #f, source-location: clause.token-src-loc);
         end if;
   infer-and-import-items(lib-annots, library, module, library, used-mod, clause)
end method;
