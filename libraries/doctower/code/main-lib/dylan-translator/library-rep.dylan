module: dylan-translator
synopsis: Code dealing specifically with library representations.


define class <library-annot> (<namespace-annotation>)
   slot annot-library :: <library>, required-init-keyword: #"library";
   slot annot-token :: false-or(<library-definer-token>) = #f,
         init-keyword: #"token";
   slot annot-modules = make(<case-insensitive-skip-list>);
end class;


define method annot-representation (annot :: <library-annot>)
=> (lib :: <library>)
   annot.annot-library
end method;


define method inferred-library? (lib :: <library>)
=> (inferred? :: <boolean>)
   instance?(lib, <unknown-library>)
end method;


//
// Library creation
//


define method make-annotated-library
   (annotations :: <skip-list>, token :: false-or(<library-definer-token>),
    library-class :: subclass(<library>), #rest keys,
    #key local-name: name :: <string>, #all-keys)
=> (library :: <library>, annotation :: <library-annot>)
   debug-assert(~key-exists?(annotations, name), "Redefinition of library");
   let new-library = apply(make, library-class, keys);
   let new-annot = make(<library-annot>, library: new-library, token: token);
   annotations[name] := new-annot;
   values(new-library, new-annot);
end method;


define method make-defined-library
   (annotations :: <skip-list>, token :: <library-definer-token>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>)
=> (library :: <library>, annotation :: <library-annot>)
   apply(make-annotated-library, annotations, token, <known-library>, keys)
end method;


define method make-inferred-library
   (annotations :: <skip-list>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>)
=> (library :: <library>, annotation :: <library-annot>)
   apply(make-annotated-library, annotations, #f, <unknown-library>, keys)
end method;


//
// Inferences and imports
//


define method infer-and-import-for-library
   (lib-annots :: <skip-list>, library :: <unknown-library>)
=> ()
   // Unknown (i.e. inferred) libraries don't import anything and don't have
   // any place to infer to.
end method;


define method infer-and-import-for-library
   (lib-annots :: <skip-list>, library :: <known-library>)
=> ()
   let annot = lib-annots[library.local-name];
   // Annot.annot-dependencies also holds use clauses, but includes use clauses
   // gathered from other libraries. We just want this library's, so reference
   // the library definition token directly.
   let use-clauses = choose(rcurry(instance?, <use-clause-token>),
                            annot.annot-token.namespace-clauses);
   do(curry(infer-and-import-library-clause, lib-annots, annot), use-clauses)
end method;


define method infer-and-import-library-clause
   (lib-annots :: <skip-list>, annot :: <library-annot>, clause :: <use-clause-token>)
=> ()
   let library = annot.annot-library;

   // Create used library if it isn't already defined.
   let used-lib-name = clause.use-name;
   let (used-lib, used-lib-annot) =
         if (key-exists?(lib-annots, used-lib-name))
            values(lib-annots[used-lib-name].annot-library,
                   lib-annots[used-lib-name])
         else
            make-inferred-library(lib-annots, local-name: used-lib-name,
                                  source-location: clause.token-src-loc);
         end if;
   
   // Process use clause.
   infer-and-import-clause(clause, used-lib.modules,

         // Make stray module in used library.
         method (name :: <string>) => ()
            make-stray-module(used-lib-annot.annot-modules, used-lib, local-name: name,
                              exported: #t, source-location: clause.token-src-loc)
         end method,

         // Make or update module.
         method (local-name :: <string>, import-name :: <string>, export :: <boolean>)
         => ()
            let existing-annot = element(annot.annot-modules, local-name, default: #f);
            let existing-module = existing-annot & existing-annot.annot-module;
            case
               existing-module & existing-module.stray? =>
                  existing-module.used-library := used-lib;
                  existing-module.import-name := import-name;
                  existing-module.exported? := existing-module.exported? | export;
               otherwise =>
                  make-imported-module(annot.annot-modules, library,
                        local-name: local-name, import-name: import-name, 
                        used-library: used-lib, exported: export,
                        source-location: clause.token-src-loc);
            end case;
         end method,

         // Note unknown reexport source.
         method () => ()
            if (used-lib.inferred-library?)
               library.unknown-reexport-sources := add-new!
                     (library.unknown-reexport-sources, used-lib, test: \=)
            end if;
         end method);
end method;
   

//
// Import propogation
//


define method import-all-for-library
   (mod-annots :: <skip-list>, library :: <unknown-library>)
=> ()
   // Unknown (i.e. inferred) libraries don't import anything.
end method;


define method import-all-for-library
   (lib-annots :: <skip-list>, library :: <known-library>)
=> ()
   let annot = lib-annots[library.local-name];
   let token = annot.annot-token;
   let use-clauses = choose(rcurry(instance?, <use-clause-token>),
                            token.namespace-clauses);
   do(curry(import-all-library-clause, lib-annots, annot), use-clauses);
end method;


define method import-all-library-clause
   (lib-annots :: <skip-list>, annot :: <library-annot>, clause :: <use-clause-token>)
=> ()
   let library = annot.annot-library;
   let used-lib = lib-annots[clause.use-name].annot-library;
   infer-and-import-clause(clause, used-lib.modules,

         // Make stray module in used library.
         always(#f),
         
         // Make or update module.
         method (local-name :: <string>, import-name :: <string>, export :: <boolean>)
         => ()
            let existing-annot = element(annot.annot-modules, local-name, default: #f);
            let existing-module = existing-annot & existing-annot.annot-module;
            case
               existing-module & existing-module.stray? =>
                  existing-module.used-library := used-lib;
                  existing-module.import-name := import-name;
                  existing-module.exported? := existing-module.exported? | export;
               ~existing-module =>
                  make-imported-module(annot.annot-modules, library,
                        local-name: local-name, import-name: import-name, 
                        used-library: used-lib, exported: export,
                        source-location: clause.token-src-loc);
            end case;
         end method,
         
         // Note unknown reexport source.
         always(#f));
end method;
