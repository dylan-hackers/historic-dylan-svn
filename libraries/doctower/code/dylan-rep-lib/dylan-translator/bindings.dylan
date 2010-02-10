module: dylan-translator


define method make-bindings-from-definition
   (context :: <context>, token :: <definition-token>)
=> ()
   let bindings = make-defined-bindings(context, token);
   
   for (binding in bindings)
      let module-name = context.context-name;
      let aliases-in-module = choose(
            method (alias :: <binding-name>) => (in-module? :: <boolean>)
               alias.enclosing-name = module-name
            end,
            binding.aliases);
      do(curry(add-definition, context, context.context-module.definitions, binding),
         aliases-in-module)
   end for;
end method;


define method make-bindings-from-export-clause
   (context :: <context>, token :: <export-clause-token>)
=> ()
   let module = context.context-module;
   for (name in token.export-names)
      let binding-name = make(<binding-name>, source-location: token.token-src-loc,
                              binding: name, within: context.context-name);
      let new-binding = make(<placeholder-binding>, local-name: binding-name, 
                             source-location: token.token-src-loc,
                             provenance: #"declaration");
      add-definition(context, module.definitions, new-binding, binding-name);
   end for;
end method;


define method make-bindings-from-create-clause
   (context :: <context>, token :: <create-clause-token>)
=> ()
   let module = context.context-module;
   for (name in token.create-names)
      let binding-name = make(<binding-name>, source-location: token.token-src-loc,
                              binding: name, within: context.context-name);
      let new-binding = make(<empty-binding>, local-name: binding-name, 
                             source-location: token.token-src-loc,
                             provenance: #"create-clause");
      add-definition(context, module.definitions, new-binding, binding-name);
   end for;
end method;


define generic make-defined-bindings
   (context :: <context>, token :: <definition-token>)
=> (bindings :: <sequence>);


define method make-expression-binding
   (context :: <context>, name :: <binding-name>)
=> (binding :: <binding>)
   make(<placeholder-binding>, local-name: name, source-location: name.source-location,
        provenance: #"expression")
end method;


define method make-inferred-binding
   (context :: <context>, name :: <binding-name>)
=> (binding :: <binding>)
   make(<placeholder-binding>, local-name: name, source-location: name.source-location,
        provenance: #"declaration")
end method;


//
// Placeholders
// 


define method merge-definitions 
   (context :: <context>, existing :: <placeholder-binding>, new :: <placeholder-binding>)
=> (merged :: <placeholder-binding>)
   let (better, worse) = better-definition(existing, new);
   unless (better == worse)
      merge-aliases(context, better, worse);
      note-replacement(context, better, worse);
   end unless;
   better
end method;


define method merge-definitions
   (context :: <context>, binding :: <binding>, placeholder :: <placeholder-binding>)
=> (merged :: <binding>)
   merge-aliases(context, binding, placeholder);
   note-replacement(context, binding, placeholder);
   binding
end method;


define method merge-definitions
   (context :: <context>, placeholder :: <placeholder-binding>, binding :: <binding>)
=> (merged :: <binding>)
   merge-definitions(context, binding, placeholder)
end method;


//
// Empty bindings
//


define method merge-definitions
   (context :: <context>, bind1 :: <empty-binding>, bind2 :: <empty-binding>)
=> (merged :: <binding>)
   let (better, worse) = better-definition(bind1, bind2);
   unless (better == worse)
      merge-aliases(context, better, worse);
      note-replacement(context, better, worse);
   end unless;
   better
end method;


//
// Binding merge helpers
//


/// Synopsis: Failsafe method. Catches illegal binding definitions.
define method merge-definitions 
   (context :: <context>, bind1 :: <binding>, bind2 :: <binding>)
=> (merged :: <binding>)
   let (better, worse) = better-definition(bind1, bind2);
   conflicting-bindings-in-module(location: worse.source-location,
         name: better.canonical-name,
         defn-locations: map(source-location, vector(better, worse)).item-string-list)
end method;


define method check-single-explicit-defn (bind1 :: <binding>, bind2 :: <binding>)
=> ()
   when (bind1.explicit-defn & bind2.explicit-defn)
      let (better, worse) = better-definition(bind1, bind2);
      let existing-defns = vector(better.explicit-defn, worse.explicit-defn);
      conflicting-bindings-in-module(location: worse.source-location,
            name: better.canonical-name,
            defn-locations: map(source-location, existing-defns).item-string-list)
   end when;
end method;
