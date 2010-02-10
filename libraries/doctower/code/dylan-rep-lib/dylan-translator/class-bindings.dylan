module: dylan-translator


define method make-defined-bindings
   (context :: <context>, token :: <class-definer-token>)
=> (bindings :: <sequence>)
   let class-name = make(<binding-name>, source-location: token.token-src-loc,
                         binding: token.api-name, within: context.context-name);
   let class-type = as(<type-fragment>, class-name);
   let class-def = make(<explicit-class-defn>, source-location: token.token-src-loc);
   let class-binding = make(<class-binding>, source-location: token.token-src-loc,
                            local-name: class-name, explicit: class-def,
                            markup: token.scoped-docs, provenance: #"definition");
   let bindings = vector(class-binding);

   // Adjectives
   map-into(class-def.adjectives, curry(as, <symbol>), token.api-modifiers);
   unless (member?(#"open", class-def.adjectives))
      class-def.adjectives := add!(class-def.adjectives, #"sealed");
   end unless;
   unless (member?(#"primary", class-def.adjectives))
      class-def.adjectives := add!(class-def.adjectives, #"free");
   end unless;
   unless (member?(#"abstract", class-def.adjectives))
      class-def.adjectives := add!(class-def.adjectives, #"concrete");
   end unless;
   class-def.adjectives := class-def.adjectives.remove-duplicates!;

   // Superclasses
   map-into(class-def.direct-supers, curry(type-fragment-from-text, context),
            token.class-supers);
   for (supertype in class-def.direct-supers)
      for (name in supertype.fragment-names)
         bindings := add!(bindings, make-expression-binding(context, name));
      end for;
   end for;
   
   // Slots
   map-into(class-def.slots,
         method (parsed-slot :: <parsed-class-slot>) => (slot :: <slot>)
            let (slot, more-bindings) = make-slot(context, class-type, parsed-slot);
            bindings := concatenate(bindings, more-bindings);
            slot
         end method, token.class-slots);
         
   // Init args
   map-into(class-def.init-args,
         method (parsed-keyword :: <class-keyword>) => (init-arg :: <init-arg>)
            let (init-arg, more-bindings) = make-init-arg(context, parsed-keyword);
            bindings := concatenate(bindings, more-bindings);
            init-arg
         end method, token.class-keywords);

   bindings
end method;


//
// Slots
//


define method make-slot
   (context :: <context>, class-type :: <type-fragment>, parsed :: <parsed-class-slot>)
=> (slot :: <slot>, bindings :: <sequence>)
   let expr-names = make(<stretchy-vector>);
   let bindings = make(<stretchy-vector>);
   let slot-mods = map(as-lowercase, parsed.slot-modifiers);
   let slot-class = <instance-slot>;
   let sealed-slot? = #f;
   for (mod in slot-mods)
      select (mod by \=)
         "instance" =>  slot-class := <instance-slot>;
         "class" =>     slot-class := <class-slot>;
         "each-subclass" => slot-class := <subclass-slot>;
         "virtual" =>   slot-class := <virtual-slot>;
         "inherited" => slot-class := <inherited-slot>;
         "sealed" =>    sealed-slot? := #t;
         otherwise =>   #f;
      end select;
   end for;
   let new-slot = make(slot-class, source-location: parsed.token-src-loc);

   when (instance?(new-slot, <initable-slot>) & parsed.slot-init)
      let spec = code-fragment-from-text(context, parsed.slot-init);
      new-slot.init-spec := spec;
      expr-names := concatenate!(expr-names, spec.fragment-names);
   end when;
   
   when (instance?(new-slot, <accessor-slot>))
      // Type
      when (parsed.slot-type)
         let type-frag = type-fragment-from-text(context, parsed.slot-type);
         new-slot.type := type-frag;
         expr-names := concatenate!(expr-names, type-frag.fragment-names);
      end when;
      
      // Getter
      let getter-name = make(<binding-name>, source-location: new-slot.source-location,
                             binding: parsed.slot-name, within: context.context-name);
      new-slot.getter := make-getter(context, new-slot, getter-name, class-type,
                                     sealed: sealed-slot?, markup: parsed.slot-doc);
      bindings := add!(bindings, new-slot.getter);

      // Setter
      if (parsed.slot-setter)
         let setter-name = make(<binding-name>,
                                source-location: new-slot.source-location,
                                binding: concatenate(parsed.slot-name, "-setter"),
                                within: context.context-name);
         new-slot.setter := make-setter(context, new-slot, setter-name, class-type,
                                        sealed: sealed-slot?, markup: parsed.slot-doc);
         bindings := add!(bindings, new-slot.setter);
      end if;
   end when;
   
   // Make other bindings.
   let more-bindings = map(curry(make-expression-binding, context), expr-names);
   bindings := concatenate!(bindings, more-bindings);
   
   values(new-slot, bindings)
end method;


define method make-getter
   (context :: <context>, slot :: <virtual-slot>, name :: <binding-name>,
    class-type :: <type-fragment>, #rest keys, #key sealed, markup)
=> (getter :: <generic-binding>)
   when (markup)
      // Can't document virtual slot, because no place to attach markup content
      // except the generic itself, which is not strictly correct.
      doc-comment-on-virtual-slot(location: markup.source-location)
   end when;
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   let value = make(<req-value>, name: "value", type: slot.type);
   apply(make-empty-generic, context, name, vector(inst-param), vector(value),
         source-location:, slot.source-location, provenance:, #"declaration", keys)
end method;


define method make-getter
   (context :: <context>, slot :: <slot>, name :: <binding-name>,
    class-type :: <type-fragment>, #rest keys, #key sealed, markup)
=> (getter :: <generic-binding>)
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   let value = make(<req-value>, name: "value", type: slot.type);
   apply(make-fixed-method, context, name, vector(inst-param), vector(value),
         source-location:, slot.source-location, provenance:, #"declaration", keys)
end method;


define method make-setter
   (context :: <context>, slot :: <virtual-slot>, name :: <binding-name>,
    class-type :: <type-fragment>, #rest keys, #key sealed, markup)
=> (setter :: <generic-binding>)
   let newval-param = make(<req-param>, name: "new-value", type: slot.type);
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   let value = make(<req-value>, name: "new-value", type: slot.type);
   apply(make-empty-generic, context, name, vector(newval-param, inst-param),
         vector(value), source-location:, slot.source-location,
         provenance:, #"declaration", keys);
end method;


define method make-setter
   (context :: <context>, slot :: <slot>, name :: <binding-name>,
    class-type :: <type-fragment>, #rest keys, #key sealed, markup)
=> (setter :: <generic-binding>)
   let newval-param = make(<req-param>, name: "new-value", type: slot.type);
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   let value = make(<req-value>, name: "new-value", type: slot.type);
   apply(make-fixed-method, context, name, vector(newval-param, inst-param),
         vector(value), source-location:, slot.source-location,
         provenance:, #"declaration", keys);
end method;


//
// Init arguments
//


define method make-init-arg
   (context :: <context>, parsed :: <class-keyword>)
=> (init-arg :: <init-arg>, bindings :: <sequence>)
   let expr-names = make(<stretchy-vector>);
   let markup = parsed.keyword-doc;
   let markup = if (markup) vector(markup) else #[] end;
   let new-init-arg = make(<init-arg>, symbol: parsed.keyword-name, markup: markup);
   
   when (parsed.keyword-type)
      let type-frag = type-fragment-from-text(context, parsed.keyword-type);
      new-init-arg.type := type-frag;
      expr-names := concatenate!(expr-names, type-frag.fragment-names);
   end when;

   when (parsed.keyword-init)
      let init-frag = code-fragment-from-text(context, parsed.keyword-init);
      new-init-arg.init-spec := init-frag;
      expr-names := concatenate!(expr-names, init-frag.fragment-names);
   end when;
   
   let bindings = map(curry(make-expression-binding, context), expr-names);
   values(new-init-arg, bindings)
end method;


//
// Merging
//


define method merge-definitions
   (context :: <context>, existing :: <class-binding>, new :: <class-binding>)
=> (merged :: <class-binding>)
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
