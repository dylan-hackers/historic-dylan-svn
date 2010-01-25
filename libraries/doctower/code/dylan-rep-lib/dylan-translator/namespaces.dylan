module: dylan-translator
synopsis: Common code dealing with library and module representations.


//
// Library creation
//


define method make-library-from-definition
   (context :: <context>, token :: <library-definer-token>)
=> (library :: <library>)
   let lib-name = make(<library-name>, source-location: token.token-src-loc,
                       library: token.api-name);
   let new-library = make(<defined-library>, source-location: token.token-src-loc,
                          local-name: lib-name, markup: token.scoped-docs,
                          provenance: #"definition");
   context.definers[new-library] := token;
   add-definition(context, context.library-definitions, new-library, lib-name)
end method;


define method make-library-from-use-clause
   (context :: <context>, token :: <use-clause-token>)
=> (library :: <library>)
   let lib-name = make(<library-name>, source-location: token.token-src-loc,
                       library: token.use-name);
   let new-library = make(<undefined-library>, source-location: token.token-src-loc,
                          local-name: lib-name, provenance: #"declaration");
   add-definition(context, context.library-definitions, new-library, lib-name)
end method;


//
// Module creation
//


define method make-module-from-definition
   (context :: <context>, token :: <module-definer-token>)
=> (module :: <module>)
   let mod-name = make(<module-name>, source-location: token.token-src-loc,
                       module: token.api-name, within: context.context-name);
   let new-module = make(<defined-module>, source-location: token.token-src-loc,
                         local-name: mod-name, markup: token.scoped-docs,
                         provenance: #"definition");
   context.definers[new-module] := token;
   add-definition(context, context.context-library.definitions, new-module, mod-name);
end method;


define method make-module-from-use-clause
   (context :: <context>, token :: <use-clause-token>)
=> (module :: <module>)
   let lib = context.context-library;
   let mod-name = make(<module-name>, module: token.use-name, 
                       within: context.context-name,
                       source-location: token.token-src-loc);
   let new-module = make(<undefined-module>, source-location: token.token-src-loc,
                         local-name: mod-name, provenance: #"declaration");
   add-definition(context, lib.definitions, new-module, mod-name);
end method;


define method make-modules-from-export-clause
   (context :: <context>, token :: <export-clause-token>)
=> ()
   let library = context.context-library;
   for (name in token.export-names)
      let mod-name = make(<module-name>, source-location: token.token-src-loc,
                          module: name, within: library.canonical-name);
      let new-module = make(<undefined-module>, source-location: token.token-src-loc,
                            local-name: mod-name, provenance: #"declaration");
      add-definition(context, library.definitions, new-module, mod-name);
   end for;
end method;


//
// Clauses
//


/// Synopsis: Populate library and module dependencies and exports.
///
/// Creates graph nodes for <defined-library> and <undefined-library>, and
/// definitions and graph nodes for <undefined-library> and <undefined-module>
/// from "use" clauses. Later, during inference stages, <undefined-module>s will
/// be replaced by <defined-module>s in other libraries if available; this is
/// not necessary for <undefined-library>, since they are merged with
/// corresponding <defined-library>s when the latter are created.
///
/// Also records "export" and "create" clause names, and builds appropriate
/// <undefined-module> and <placeholder-binding> objects.
///
/// This does not handle modules and bindings listed in library or module "use"
/// clause options, such as "import:" and "rename". They are handled during
/// inferencing.
///
define method process-namespace-clauses (context :: <context>) => ()
   // Do libraries. Use shallow-copy in case clauses add new definitions.
   for (lib in shallow-copy(context.library-definitions))

      // Add node for library definition.
      let node = find-node-by-value(context.library-graph, lib)
            | make(<node>, graph: context.library-graph, value: lib);

      // Evaluate clauses if the library has a definer.
      when (key-exists?(context.definers, lib))
         let clauses = context.definers[lib].namespace-clauses;
         with-context-name(lib.canonical-name)
            do(curry(process-namespace-clause, context, lib, node), clauses);

            // Do library's modules. Use shallow-copy in case clauses add new
            // definitions. 
            for (mod keyed-by local-name in shallow-copy(lib.definitions))
            
               // Add node for module definition.
               let node = find-node-by-value(context.module-graph, mod)
                     | make(<node>, graph: context.module-graph, value: mod);
               
               // Evaluate clauses if the module has a definer.
               when (key-exists?(context.definers, mod))
                  let clauses = context.definers[mod].namespace-clauses;
                  let module-name = make(<module-name>, module: local-name,
                                         within: context.context-name);
                  with-context-name(module-name)
                     do(curry(process-namespace-clause, context, mod, node), clauses);
                  end with-context-name;
               end when;
            end for;
         end with-context-name;
      end when;
   end for;
end method;


define method process-namespace-clause
   (context :: <context>, library :: <library>, node :: <node>, 
    clause :: <export-clause-token>)
=> ()
   make-modules-from-export-clause(context, clause);
   library.exported-names := union(library.exported-names, clause.export-names,
                                   test: case-insensitive-equal?);
end method;


define method process-namespace-clause
   (context :: <context>, module :: <module>, node :: <node>, 
    clause :: <export-clause-token>)
=> ()
   make-bindings-from-export-clause(context, clause);
   module.exported-names := union(module.exported-names, clause.export-names,
                                  test: case-insensitive-equal?);
end method;


define method process-namespace-clause
   (context :: <context>, module :: <module>, node :: <node>, 
    clause :: <create-clause-token>)
=> ()
   make-bindings-from-create-clause(context, clause);
   module.exported-names := union(module.exported-names, clause.create-names,
                                  test: case-insensitive-equal?);
end method;


define method process-namespace-clause
   (context :: <context>, library :: <library>, node :: <node>, 
    clause :: <use-clause-token>)
=> ()
   let used-lib = make-library-from-use-clause(context, clause);
   let used-node = find-node-by-value(context.library-graph, used-lib)
         | make(<node>, graph: context.library-graph, value: used-lib);
   connect(node, used-node, label: clause.use-name);
end method;


define method process-namespace-clause 
   (context :: <context>, module :: <module>, node :: <node>, 
    clause :: <use-clause-token>)
=> ()
   let lib = context.context-library;
   let used-mod = make-module-from-use-clause(context, clause);
   let used-node = find-node-by-value(context.module-graph, used-mod)
         | make(<node>, graph: context.module-graph, value: used-mod);
   connect(node, used-node, label: clause.use-name);
end method;


//
// General namespace merging
//


define method merge-definitions
   (context :: <context>, existing :: <namespace>, new :: <namespace>)
=> (merged :: <namespace>)
   let (better, worse) = better-definition(existing, new);
   unless (better == worse)
      // /**/ log("merging %= into %=", worse, better);
      // /**/ log("  %= exports %=", better.canonical-name, better.exported-names);
      // /**/ log("  %= exports %=", worse.canonical-name, worse.exported-names);
      better.definitions := map-into(better.definitions, 
                                     curry(merge-definitions, context),
                                     better.definitions, worse.definitions);

      better.markup-tokens := union(better.markup-tokens, worse.markup-tokens,
                                    test: \=);
   
      merge-exported-names(context, better, worse);
      merge-aliases(context, better, worse);
      note-replacement(context, better, worse);
      // /**/ log("  now %= exports %=", better.canonical-name, better.exported-names);
   end unless;
   better
end method;


define method merge-definitions
   (context :: <context>, existing :: <defined-library>, new :: <defined-library>)
=> (merged :: <defined-library>)
   unless (existing == new)
      let locs = map(source-location, vector(new, existing));
      conflicting-libraries-in-filesets(location: new.source-location,
            name: new.canonical-name.local-name, defn-locations: locs.item-string-list);
   end unless;
   existing
end method;


define method merge-definitions
   (context :: <context>, existing :: <defined-module>, new :: <defined-module>)
=> (merged :: <defined-module>)
   unless (existing == new)
      let locs = map(source-location, vector(new, existing));
      conflicting-modules-in-library(location: new.source-location,
            name: new.canonical-name.local-name, defn-locations: locs.item-string-list);
   end unless;
   existing
end method;


/// Synopsis: Failsafe method.
define method merge-definitions
   (context :: <context>, namespace1 :: <library>, namespace2 :: <module>)
=> (merged :: <namespace>)
   error("Trying to merge library and module")
end method;


/// Synopsis: Failsafe method.
define method merge-definitions
   (context :: <context>, namespace1 :: <module>, namespace2 :: <library>)
=> (merged :: <namespace>)
   error("Trying to merge library and module")
end method;


define method merge-exported-names
   (context :: <context>, better :: <namespace>, worse :: <namespace>)
=> ()
   let preunion-size = better.exported-names.size;
   better.exported-names := union(better.exported-names, worse.exported-names,
                                  test: case-insensitive-equal?);
   if (better.exported-names.size > preunion-size)
      context.changed? := #t;
   end if;
end method;
