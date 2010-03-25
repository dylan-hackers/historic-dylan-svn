module: dylan-translator


define method make-defined-bindings
   (context :: <context>, token :: <generic-definer-token>)
=> (bindings :: <sequence>)
   let local-name = make(<binding-name>, source-location: token.token-src-loc,
                         binding: token.api-name, within: context.context-name);
   let func-def = make(<explicit-generic-defn>, source-location: token.token-src-loc);
   let generic-binding = make(<generic-binding>, source-location: token.token-src-loc,
                              local-name: local-name, explicit: func-def,
                              provenance: #"generic-definition",
                              markup: token.scoped-docs);

   // Adjectives
   map-into(func-def.adjectives, curry(as, <symbol>), token.api-modifiers);
   if (member?(#"open", func-def.adjectives))
      generic-binding.sealed? := #f;
   else
      func-def.adjectives := add!(func-def.adjectives, #"sealed");
   end if;
   func-def.adjectives := func-def.adjectives.remove-duplicates!;
   
   // Parameter list
   let (func-params, param-bindings) = make-param-list(context, token.func-params);
   let (func-values, value-bindings) = make-value-list(context, token.func-values);
   func-def.param-list := func-params;
   func-def.value-list := func-values;
   
   // Vendor options
   map-into(func-def.vendor-options, curry(as, <vendor-option>), token.func-options);

   concatenate(vector(generic-binding), param-bindings, value-bindings)
end method;


define method make-defined-bindings
   (context :: <context>, token :: <method-definer-token>)
=> (bindings :: <sequence>)
   let local-name = make(<binding-name>, source-location: token.token-src-loc,
                         binding: token.api-name, within: context.context-name);
   let func-def = make(<implicit-generic-defn>, source-location: token.token-src-loc,
                       markup: token.scoped-docs);
   let generic-binding = make(<generic-binding>, source-location: token.token-src-loc,
                              local-name: local-name, implicit: vector(func-def),
                              provenance: #"definition");

   // Adjectives
   map-into(func-def.adjectives, curry(as, <symbol>), token.api-modifiers);
   func-def.adjectives := func-def.adjectives.remove-duplicates!;
   
   // Parameter list
   let (func-params, param-bindings) = make-param-list(context, token.func-params);
   let (func-values, value-bindings) = make-value-list(context, token.func-values);
   func-def.param-list := func-params;
   func-def.value-list := func-values;
   
   // Sealed domain
   if (member?(#"sealed", func-def.adjectives))
      seal-generic(generic-binding, map(type, func-params.req-params));
      func-def.adjectives := remove!(func-def.adjectives, #"sealed");
   end if;

   concatenate(vector(generic-binding), param-bindings, value-bindings)
end method;


define method make-defined-bindings
   (context :: <context>, token :: <function-definer-token>)
=> (bindings :: <sequence>)
   let local-name = make(<binding-name>, source-location: token.token-src-loc,
                         binding: token.api-name, within: context.context-name);
   let func-def = make(<explicit-function-defn>, source-location: token.token-src-loc);
   let function-binding = make(<function-binding>, source-location: token.token-src-loc,
                               local-name: local-name, explicit: func-def,
                               markup: token.scoped-docs, provenance: #"definition");

   // Adjectives
   map-into(func-def.adjectives, curry(as, <symbol>), token.api-modifiers);
   func-def.adjectives := func-def.adjectives.remove-duplicates!;

   // Parameter list
   let (func-params, param-bindings) = make-param-list(context, token.func-params);
   let (func-values, value-bindings) = make-value-list(context, token.func-values);
   func-def.param-list := func-params;
   func-def.value-list := func-values;
   
   concatenate(vector(function-binding), param-bindings, value-bindings);
end method;


define method make-defined-bindings
   (context :: <context>, token :: <domain-definer-token>)
=> (bindings :: <sequence>)
   let local-name = make(<binding-name>, source-location: token.token-src-loc,
                         binding: token.api-name, within: context.context-name);
   let generic = make(<generic-binding>, source-location: token.token-src-loc,
                      local-name: local-name, explicit: #f, implicit: #[],
                      provenance: #"declaration");
   let types = map(curry(type-fragment-from-text, context), token.domain-types);
   let names = map(fragment-names, types);
   seal-generic(generic, types);
   map(curry(make-expression-binding, context), names)
end method;


define method make-param-list
   (context :: <context>, parsed-params :: <sequence>)
=> (param-list :: <param-list>, bindings :: <sequence>)
   let param-list-class =
         case
            any?(rcurry(instance?, <accepts-keys-argument>), parsed-params) =>
               <key-param-list>;
            any?(rcurry(instance?, <rest-argument>), parsed-params) =>
               <var-param-list>;
            otherwise =>
               <fixed-param-list>;
         end case;

   let param-list = make(param-list-class);
   let expr-names = make(<stretchy-vector>);
   for (param in parsed-params)
      let markup = param.param-doc;
      let markup = if (markup) vector(markup) else #[] end;

      select (param by instance?)
         <required-typed-argument> =>
            let param-rep = make(<req-param>, name: param.param-name,
                                 markup: markup);
            when (param.param-type)
               let type-frag = type-fragment-from-text(context, param.param-type);
               param-rep.type := type-frag;
               expr-names := concatenate!(expr-names, type-frag.fragment-names);
            end when;
            param-list.req-params := add!(param-list.req-params, param-rep);

         <required-singleton-argument> =>
            let param-rep = make(<req-param>, name: param.param-name,
                                 markup: markup);
            when (param.param-instance)
               let type-frag = singleton-from-text(context, param.param-instance);
               param-rep.type := type-frag;
               expr-names := concatenate!(expr-names, type-frag.fragment-names);
            end when;
            param-list.req-params := add!(param-list.req-params, param-rep);

         <rest-argument> =>
            let param-rep = make(<rest-param>, name: param.param-name,
                                 markup: markup);
            param-list.rest-param := param-rep;

         <keyword-argument> =>
            let param-rep = make(<key-param>, name: param.param-name,
                                 markup: markup);
            when (param.param-type)
               let type-frag = type-fragment-from-text(context, param.param-type);
               param-rep.type := type-frag;
               expr-names := concatenate!(expr-names, type-frag.fragment-names);
            end when;
            when (param.param-default)
               let code-frag = code-fragment-from-text(context, param.param-default);
               param-rep.expr := code-frag;
               expr-names := concatenate!(expr-names, code-frag.fragment-names);
            end when;
            param-list.key-params := add!(param-list.key-params, param-rep);

         <accepts-keys-argument> =>
            #f; // Do nothing; this only marks #key.

         <all-keys-argument> =>
            param-list.all-keys? := #t;
      end select;
   end for;
   let bindings = map(curry(make-expression-binding, context), expr-names);
   values(param-list, bindings)
end method;


define method make-value-list
   (context :: <context>, parsed-vals :: <sequence> /* of <func-value> */)
=> (value-list :: <value-list>, name-tokens :: <sequence>)
   let value-list = make(<value-list>);
   let expr-names = make(<stretchy-vector>);
   for (val in parsed-vals)
      let markup = val.param-doc;
      let markup = if (markup) vector(markup) else #[] end;

      select (val by instance?)
         <required-value> =>
            let value-rep = make(<req-value>, name: val.param-name,
                                 markup: markup);
            when (val.param-type)
               let type-frag = type-fragment-from-text(context, val.param-type);
               value-rep.type := type-frag;
               expr-names := concatenate!(expr-names, type-frag.fragment-names);
            end when;
            value-list.req-values := add!(value-list.req-values, value-rep);

         <parsed-rest-value> =>
            let value-rep = make(<rest-value>, name: val.param-name,
                                 markup: markup);
            when (val.param-type)
               let type-frag = type-fragment-from-text(context, val.param-type);
               value-rep.type := type-frag;
               expr-names := concatenate!(expr-names, type-frag.fragment-names);
            end when;
            value-list.rest-value := value-rep;
      end select;
   end for;
   let bindings = map(curry(make-expression-binding, context), expr-names);
   values(value-list, bindings);
end method;


define method make-fixed-method
   (context :: <context>, name :: <binding-name>,
    meth-params :: <sequence>, meth-values :: <sequence>,
    #key markup-token :: false-or(<markup-content-token>) = #f,
         sealed: sealed? :: <boolean> = #f, provenance :: <symbol>,
         source-location :: <source-location> = name.source-location)
=> (generic :: <generic-binding>)
   let meth-param-list = make(<fixed-param-list>, req-params: meth-params);
   let meth-value-list = make(<value-list>, req-values: meth-values);
   let markup = if (markup-token) vector(markup-token) else #[] end;
   let impl-def = make(<implicit-generic-defn>, source-location: source-location,
                       param-list: meth-param-list, value-list: meth-value-list,
                       markup: markup);
   let generic = make(<generic-binding>, source-location: source-location,
                      local-name: name, implicit: vector(impl-def),
                      provenance: provenance);
   if (sealed?)
      seal-generic(generic, map(type, meth-params))
   end if;
   generic
end method;


define method make-empty-generic
   (context :: <context>, name :: <binding-name>,
    meth-params :: <sequence>, meth-values :: <sequence>,
    #key markup-token :: false-or(<markup-content-token>) = #f,
         sealed: sealed? :: <boolean> = #f, provenance :: <symbol>,
         source-location :: <source-location> = name.source-location)
=> (generic :: <generic-binding>)
   // Markup is ignored because it only comes with a defining macro, and that
   // would imply a non-empty generic.
   let generic = make(<generic-binding>, source-location: source-location,
                      local-name: name, explicit: #f, implicit: #[],
                      provenance: provenance);
   if (sealed?)
      seal-generic(generic, map(type, meth-params))
   end if;
   generic
end method;


define method seal-generic (generic :: <generic-binding>, types :: <sequence>)
=> ()
   let types = types.copy-sequence;
   replace-elements!(types, false?, always($object-type));
   let sealed-spec = make(<sealed-domain>, types: types);
   generic.sealed-domains := add-new!(generic.sealed-domains, sealed-spec, test: \=);
end method;


//
// Merging
//


define method merge-definitions
   (context :: <context>, existing :: <generic-binding>, new :: <generic-binding>)
=> (merged :: <generic-binding>)
   let (better, worse) = better-definition(existing, new);
   unless (better == worse)
      check-single-explicit-defn(better, worse);
      better.explicit-defn := better.explicit-defn | worse.explicit-defn;
      
      better.implicit-defns := union(better.implicit-defns, worse.implicit-defns);

      better.sealed-domains := union(better.sealed-domains, worse.sealed-domains,
                                     test: \=);

      better.sealed? := better.sealed? | worse.sealed?;
      
      merge-aliases(context, better, worse);
      merge-markup(context, better, worse);
      note-replacement(context, better, worse);
   end unless;
   better
end method;


define method merge-definitions
   (context :: <context>, existing :: <function-binding>, new :: <function-binding>)
=> (merged :: <function-binding>)
   let (better, worse) = better-definition(existing, new);
   unless (better == worse)
      check-single-explicit-defn(better, worse);
      better.explicit-defn := better.explicit-defn | worse.explicit-defn;
      
      merge-aliases(context, better, worse);
      merge-markup(context, better, worse);
      note-replacement(context, better, worse);
   end unless;
   better
end method;
   