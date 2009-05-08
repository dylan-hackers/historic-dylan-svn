module: dylan-translator
synopsis: Code dealing specifically with library representations.


define class <library-annot> (<namespace-annotation>)
   slot annot-library :: <library>, required-init-keyword: #"library";
   slot annot-token :: false-or(<library-definer-token>) = #f,
         init-keyword: #"token";
   slot annot-modules = make(<case-insensitive-skip-list>, size: 25);
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
   let new-library = apply(make, library-class, keys);
   let new-annot = make(<library-annot>, library: new-library, token: token);
   annotations[name] := new-annot;
   values(new-library, new-annot);
end method;


define method make-defined-library
   (annotations :: <skip-list>, token :: <library-definer-token>, #rest keys,
    #key local-name :: <string>, source-location :: <source-location>)
=> (library :: <library>, annotation :: <library-annot>)
   apply(make-annotated-library, annotations, token, <known-library>,
         markup: token.scoped-docs, keys)
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
   // Annot.annot-dependencies also holds use clauses, but includes use clauses
   // gathered from other libraries. We just want this library's, so reference
   // the library definition token directly.
   // log("Promote/import modules in %s", library.local-name);
   let annot = lib-annots[library.local-name];
   let use-clauses = choose(rcurry(instance?, <use-clause-token>),
                            annot.annot-token.namespace-clauses);
   do(curry(infer-and-import-library-clause, lib-annots, annot), use-clauses)
end method;


define method infer-and-import-library-clause
   (lib-annots :: <skip-list>, annot :: <library-annot>, clause :: <use-clause-token>)
=> ()
   let library = annot.annot-library;
   let used-lib-name = clause.use-name;
   let (used-lib, used-lib-annot) =
         if (key-exists?(lib-annots, used-lib-name))
            values(lib-annots[used-lib-name].annot-library,
                   lib-annots[used-lib-name])
         else
            // log("  Infer used library %s", used-lib-name);
            make-inferred-library(lib-annots, local-name: used-lib-name,
                                  source-location: clause.token-src-loc);
         end if;
   infer-and-import-items(lib-annots, library, #f, used-lib, #f, clause)
end method;
