module: dylan-translator
synopsis: Code for constant, variable, and macro bindings.


//
// Variables/constants
//


define method make-defined-bindings
   (context :: <context>, token :: <variable-definer-token>)
=> (bindings :: <sequence>)
   let local-name = make(<binding-name>, source-location: token.token-src-loc,
                         binding: token.api-name, within: context.context-name);
   let type-frag = token.api-type & type-fragment-from-text(context, token.api-type);
   let value-frag = computed-constant-from-text(context, token.api-value);
   make-defined-var/const-bindings(context, local-name, type-frag, value-frag,
         <explicit-variable-defn>, <variable-binding>, token)
end method;


/// A "define constant" where the value is a single scoped name is actually an
/// alias for the value. Its type must be a superclass of the value's type, but
/// we do not verify such things, so we will ignore it.
define method make-defined-bindings
   (context :: <context>, token :: <constant-definer-token>)
=> (bindings :: <sequence>)
   let local-name = make(<binding-name>, source-location: token.token-src-loc,
                         binding: token.api-name, within: context.context-name);
   let type-frag = token.api-type & type-fragment-from-text(context, token.api-type);
   let value-frag = computed-constant-from-text(context, token.api-value);
   if (value-frag.simple-name?)
      let value-name = value-frag.fragment-names.first;

      // An alias may not be documented in isolation. Only the actual definition, 
      // or an explicit topic, can supply documentation.
      let token-comment = ~token.scoped-docs.empty? & token.scoped-docs.first;
      when (token-comment & ~token-comment.default-topic-content.empty?)
         doc-comment-on-binding-alias(location: token.token-src-loc,
               alias-name: value-name);
      end when;
      
      // Alias
      let binding = make(<placeholder-binding>, local-name: local-name,
            source-location: token.token-src-loc, provenance: #"declaration",
            markup: token.scoped-docs);
      binding.aliases := add!(binding.aliases, value-name);
      let bindings = vector(binding);

      // Type names
      when (type-frag)
         for (name in type-frag.fragment-names)
            bindings := add!(bindings, make-expression-binding(context, name));
         end for;
      end when;
      
      bindings
   else
      make-defined-var/const-bindings(context, local-name, type-frag, value-frag,
            <explicit-variable-defn>, <variable-binding>, token);
   end if
end method;


define method make-const-alias-bindings
   (context :: <context>, token :: <constant-definer-token>)
=> (bindings :: <sequence>)
end method;


define method make-defined-var/const-bindings
   (context :: <context>, local-name :: <binding-name>, 
    type-frag :: false-or(<type-fragment>), value-frag :: <computed-constant>,
    defn-class :: <class>, binding-class :: <class>,
    token :: type-union(<variable-definer-token>, <constant-definer-token>))
=> (bindings :: <sequence>)
   let binding-defn = make(defn-class, source-location: token.token-src-loc,
                           type: type-frag, value: value-frag);
   let binding = make(binding-class, source-location: token.token-src-loc,
                      local-name: local-name, explicit: binding-defn,
                      provenance: #"definition", markup: token.scoped-docs);
   let bindings = vector(binding);

   // Adjectives
   map-into(binding-defn.adjectives, curry(as, <symbol>), token.api-modifiers);
   binding-defn.adjectives := binding-defn.adjectives.remove-duplicates!;

   // Type names
   when (type-frag)
      for (name in type-frag.fragment-names)
         bindings := add!(bindings, make-expression-binding(context, name));
      end for;
   end when;

   // Value names
   for (name in value-frag.fragment-names)
      bindings := add!(bindings, make-expression-binding(context, name));
   end for;
   
   bindings
end method;


define method merge-definitions
   (context :: <context>, existing :: <constant-binding>, new :: <constant-binding>)
=> (merged :: <constant-binding>)
   let (better, worse) = better-definition(existing, new);
   unless (better == worse)
      check-single-explicit-defn(better, worse);
      better.explicit-defn := better.explicit-defn | worse.explicit-defn;
      
      merge-aliases(context, better, worse);
      note-replacement(context, better, worse);
   end unless;
   better
end method;


define method merge-definitions
   (context :: <context>, existing :: <variable-binding>, new :: <variable-binding>)
=> (merged :: <variable-binding>)
   let (better, worse) = better-definition(existing, new);
   unless (better == worse)
      check-single-explicit-defn(better, worse);
      better.explicit-defn := better.explicit-defn | worse.explicit-defn;
      
      merge-aliases(context, better, worse);
      note-replacement(context, better, worse);
   end unless;
   better
end method;


//
// Macros
//


define method make-defined-bindings
   (context :: <context>, token :: <macro-definer-token>)
=> (bindings :: <sequence>)
   let main-rules = token.main-rule-set;
   let defn-class =
         case
            every?(rcurry(instance?, <body-style-definition-rule-token>), main-rules)
               => <explicit-body-macro-defn>;
            every?(rcurry(instance?, <list-style-definition-rule-token>), main-rules)
               => <explicit-list-macro-defn>;
            every?(rcurry(instance?, <statement-rule-token>), main-rules)
               => <explicit-stmt-macro-defn>;
            every?(rcurry(instance?, <function-rule-token>), main-rules)
               => <explicit-func-macro-defn>;
         end case;

   let binding-name = make(<binding-name>, source-location: token.token-src-loc,
                           binding: token.api-name, within: context.context-name);
   let binding-defn = make(defn-class, source-location: token.token-src-loc);
   let binding = make(<macro-binding>, source-location: token.token-src-loc,
                      local-name: binding-name, explicit: binding-defn,
                      provenance: #"definition", markup: token.scoped-docs);
   vector(binding);
end method;


define method merge-definitions
   (context :: <context>, existing :: <macro-binding>, new :: <macro-binding>)
=> (merged :: <macro-binding>)
   let (better, worse) = better-definition(existing, new);
   unless (better == worse)
      check-single-explicit-defn(better, worse);
      better.explicit-defn := better.explicit-defn | worse.explicit-defn;
      
      merge-aliases(context, better, worse);
      note-replacement(context, better, worse);
   end unless;
   better
end method;

