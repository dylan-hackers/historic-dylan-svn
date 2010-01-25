module: dylan-translator
synopsis: Code to consolidate, merge, and infer definitions and bindings created
          in the first round of parser token translation.


define method infer-and-merge-definitions (context :: <context>) => ()
   clean-up-definitions(context);
   while (context.changed?)
      context.changed? := #f;
      do(curry(infer-and-merge-node, context), sorted-graph(context.library-graph));
      do(curry(infer-and-merge-node, context), sorted-graph(context.module-graph));
      clean-up-definitions(context);
   end while;
end method;


define method infer-and-merge-node (context :: <context>, node :: <node>) => ()
   let defn :: <namespace> = node.value;
   let definer = element(context.definers, defn, default: #f);
   if (definer)
      let clauses = choose(rcurry(instance?, <use-clause-token>),
            context.definers[defn].namespace-clauses);
      for (use-clause :: <use-clause-token> in clauses)
         let used-local-name = use-clause.use-name;
         let used-node :: <node> = find-target-node-by-edge-label
               (node, used-local-name, test: case-insensitive-equal?);
         let used-defn :: <namespace> = used-node.value;
         infer-and-merge-used-names(context, defn, used-defn, use-clause);
      end for;
   else
      // Without a use clause, we cannot tell which of the namespace's
      // definitions correspond to a definition in its used namespace. We DO
      // have a list of the namespace's exported names, so we could push them to
      // other namespaces that are using this one, but since we are crawling the
      // graph from "using" to "used," we will already have been to the
      // namespaces using this one. So do nothing.
      #f
   end if;
end method;


//
// Use clause handling
//


define method infer-and-merge-used-names
   (context :: <context>, namespace :: <defined-namespace>,
    used-namespace :: <namespace>, clause :: <use-clause-token>)
=> ()
   // /**/ log("infer on %= using %=", namespace, used-namespace);

   // Interpret use clause options.

   let import-options :: type-union(<sequence> /* of <renaming-token> */,
                                    singleton(#"all"))
         = (clause & clause.use-imports) | #"all";
   let rename-options :: <sequence> /* of <renaming-token> */
         = (clause & clause.use-renamings) | #[];
   let exclude-options :: <sequence> /* of <string> */
         = (clause & clause.use-exclusions) | #[];
   let export-options :: type-union(<sequence> /* of <string> */, singleton(#"all"))
         = if (clause) clause.use-exports | #[] else #"all" end;
   let prefix-option :: false-or(<string>)
         = clause & clause.use-prefix;
   
   let import-and-rename-options :: <sequence> /* of <renaming-token> */
         = concatenate(if (import-options = #"all") #[] else import-options end,
                       rename-options);

   // Figure used & local names for expected definitions.
   
   let names-in-used 
         = as(<stretchy-vector>, used-namespace.exported-names.copy-sequence);
   let names-in-this = make(<stretchy-vector>, size: names-in-used.size, fill: #f);

   local method find-used-name-index (name :: <string>)
         => (index :: false-or(<integer>))
            find-key(names-in-used, curry(case-insensitive-equal?, name))
         end,
         
         method set-name-for-used-name (this-name, used-name :: <string>)
         => ()
            let name-index = find-used-name-index(used-name);
            if (name-index)
               names-in-this[name-index] := this-name;
            else
               add!(names-in-used, used-name);
               add!(names-in-this, this-name);
            end if;
         end;
               
   // Include names from export specifiers...
   if (export-options ~= #"all")
      for (reexported-name in export-options)
         let name-in-used = unprefixed-name(reexported-name, prefix-option);
         if (name-in-used)
            set-name-for-used-name(reexported-name, name-in-used);
         end if;
      end for;
   end if;
   
   // ...and maybe all exported names from used namespace...
   if (import-options = #"all")
      replace-elements!(names-in-this, false?, always(#t));
   end if;
   
   // ...but exclude excluded names...
   for (excluded-name in exclude-options)
      set-name-for-used-name(#f, excluded-name);
   end for;
   
   // ...and include specifically-mentioned imported and renamed names...
   for (renaming-token :: <renaming-token> in import-and-rename-options)
      let name-in-used = renaming-token.token-import-name;
      let name-in-this = renaming-token.token-local-name | #t;
      set-name-for-used-name(name-in-this, name-in-used);
   end for;
   
   // ...either with prefixes or as themselves.
   for (name-in-used keyed-by name-index in names-in-used)
      if (names-in-this[name-index] = #t)
         names-in-this[name-index] := concatenate(prefix-option | "", name-in-used);
      end if;
   end for;
   
   // /**/ log("  names in used %=", names-in-used);
   // /**/ log("  names in this %=", names-in-this);
   
   // Skip names we don't care about.
   let imported-names-from-used = choose-by(true?, names-in-this, names-in-used);
   let imported-names-in-local = choose(true?, names-in-this);
   
   // Go through each local/imported name pairing and create placeholders or merge.
   
   for (this-name in imported-names-in-local, used-name in imported-names-from-used)
      let this-defn = element(namespace.definitions, this-name, default: #f)
            | make-inferred-definition-in-namespace(namespace, this-name, clause);
      let used-defn = element(used-namespace.definitions, used-name, default: #f)
            | make-inferred-definition-in-namespace(used-namespace, used-name, clause);
      let merged-defn = merge-definitions(context, this-defn, used-defn);
      
      let this-scoped-name = make-name-in-namespace(namespace, this-name, clause);
      let used-scoped-name = make-name-in-namespace(used-namespace, used-name, clause);
      
      add-definition(context, namespace.definitions, merged-defn,
                     this-scoped-name);
      add-definition(context, used-namespace.definitions, merged-defn,
                     used-scoped-name);
   end for;
   
   // Add any new exported names that we find.
   
   let used-names-to-export
         = if (export-options = #"all") imported-names-in-local
           else export-options end if;
   let new-names-to-export = difference(used-names-to-export, namespace.exported-names,
                                        test: case-insensitive-equal?);

   if (new-names-to-export.size > 0)
      context.changed? := #t;
      namespace.exported-names
            := concatenate!(namespace.exported-names, new-names-to-export);
   end if;
   
   // Add used namespace's unknown reexport sources, if applicable.
   
   if (import-options = #"all" & export-options = #"all")
      if (used-namespace.unknown-reexport-source?)
         namespace.unknown-reexport-sources 
               := add-new!(namespace.unknown-reexport-sources, used-namespace)
      elseif (instance?(used-namespace, <defined-namespace>))
         namespace.unknown-reexport-sources
               := union(namespace.unknown-reexport-sources,
                        used-namespace.unknown-reexport-sources)
      end if;
   end if;
end method;


define method unprefixed-name (name :: <string>, prefix :: false-or(<string>))
=> (name :: false-or(<string>))
   if (prefix)
      let maybe-prefix = copy-sequence(name, end: prefix.size);
      if (case-insensitive-equal?(maybe-prefix, prefix))
         copy-sequence(name, start: prefix.size)
      end if
   else
      name
   end if
end method;


define method make-name-in-namespace
   (library :: <library>, local-name :: <string>, token :: <token>)
=> (scoped-name :: <module-name>)
   make(<module-name>, module: local-name, within: library.canonical-name,
        source-location: token.token-src-loc)
end method;


define method make-name-in-namespace 
   (module :: <module>, local-name :: <string>, token :: <token>)
=> (scoped-name :: <binding-name>)
   make(<binding-name>, binding: local-name, within: module.canonical-name,
        source-location: token.token-src-loc)
end method;


define method make-inferred-definition-in-namespace
   (library :: <library>, local-name :: <string>, token :: <token>)
=> (defn :: <module>)
   let scoped-name = make-name-in-namespace(library, local-name, token);
   make(<undefined-module>, local-name: scoped-name, provenance: #"inference",
        source-location: token.token-src-loc);
end method;


define method make-inferred-definition-in-namespace
   (module :: <module>, local-name :: <string>, token :: <token>)
=> (defn :: <binding>)
   let scoped-name = make-name-in-namespace(module, local-name, token);
   make(<placeholder-binding>, local-name: scoped-name, provenance: #"inference",
        source-location: token.token-src-loc);
end method;


//
// Clean up
//


define method clean-up-definitions (context :: <context>) => ()

   // Replace definitions and other references in libraries and modules.
   replace-in-collection(context, context.library-definitions);
   for (lib in context.library-definitions)
      clean-up-reexport-sources(context, lib);
      replace-in-collection(context, lib.definitions);
      for (mod in lib.definitions)
         clean-up-reexport-sources(context, mod);
         replace-in-collection(context, mod.definitions);
      end for;
   end for;
   
   // Replace graph node values and check for cycles.
   clean-up-graph(context, context.library-graph);
   clean-up-graph(context, context.module-graph);
   
   // Finished with replacements.
   remove-all-keys!(context.replacements);
end method;


define method replace-in-collection (context :: <context>, coll :: <collection>)
=> ()
   replace-elements!(coll,
         curry(key-exists?, context.replacements),
         curry(element, context.replacements))
end method;


define method clean-up-reexport-sources
   (context :: <context>, namespace :: <defined-namespace>)
=> ()
   replace-in-collection(context, namespace.unknown-reexport-sources);
   namespace.unknown-reexport-sources 
         := choose(unknown-reexport-source?, namespace.unknown-reexport-sources);
end method;


define method clean-up-reexport-sources
   (context :: <context>, namespace :: <undefined-namespace>)
=> ()
   #f
end method;


define function unknown-reexport-source? (namespace :: <namespace>)
=> (source? :: <boolean>)
   instance?(namespace, <undefined-namespace>)
   & ~(namespace == $dylan-library | namespace == $dylan-module)
end function;


define method clean-up-graph (context :: <context>, graph :: <graph>) => ()

   // Replace node values.
   
   for (node in graph.nodes)
      let new = element(context.replacements, node.value, default: #f);
      if (new) node.value := new end;
   end for;
   
   // Nodes that now share a value must be merged.
   
   local method common-value? (node1 :: <node>, node2 :: <node>) 
         => (common? :: <boolean>)
            node1.value == node2.value
         end,
         
         method merge-nodes (node1 :: <node>, node2 :: <node>) => (node :: <node>)
            for (e :: <edge> in node2.outgoing-edges)
               let node2-target = e.edge-target;
               connect(node1, node2-target, label: e.edge-label);
               disconnect(node2, node2-target);
            end for;
            for (e :: <edge> in node2.incoming-edges)
               let node2-source = e.edge-source;
               connect(node2-source, node1, label: e.edge-label);
               disconnect(node2-source, node2);
            end for;
            remove-node(node2);
            node1
         end;
         
   let groups = group-elements(graph.nodes, test: common-value?);
   let sizes = map(size, groups);
   let duplicates = choose-by(rcurry(\>, 1), sizes, groups);
   do(curry(reduce1, merge-nodes), duplicates);

   // Make sure we haven't got any duplicate edges.
   
   let edge-groups = group-elements(graph.edges, test:
         method (e1 :: <edge>, e2 :: <edge>) => (dup? :: <boolean>)
            e1.edge-source == e2.edge-source & e1.edge-target == e2.edge-target
                  & case-insensitive-equal?(e1.edge-label, e2.edge-label)
         end);

   for (group :: <sequence> in edge-groups)
      do(remove-edge, copy-sequence(group, start: 1))
   end for;

   // Check for cycles.
   
   if (~graph.acyclic?)
      visit-cycles(graph, graph-cycle-error)
   end if;
end method;


define method graph-cycle-error (cycle :: <sequence>)
   local method def-< (def1 :: <definition>, def2 :: <definition>)
            let (better, worse) = better-definition(def1, def2);
            better == def2
         end;
         
   let defns = map(value, cycle);
   let sorted-defns = sort(defns, test: def-<);
   let first-defn = sorted-defns.first;
   
   circular-definition(location: first-defn.source-location,
         name: first-defn.canonical-name.local-name,
         defn-locations: map(source-location, sorted-defns).item-string-list);
end method;

