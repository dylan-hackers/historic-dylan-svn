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
      // Can't assume that a simple name refers to a class; it could be a variable.
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

   // Getter
   let slot-name = make(<binding-name>, source-location: new-slot.source-location,
                        binding: parsed.token-slot-name, within: context.context-name);
   new-slot.getter := make-getter(context, new-slot, slot-name, class-type,
                                  sealed: sealed-slot?, markup-token: parsed.slot-doc);
   bindings := add!(bindings, new-slot.getter);

   when (instance?(new-slot, <initable-slot>) & parsed.slot-init)
      // Init spec
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
      
      // Setter
      if (parsed.token-slot-setter)
         let setter-name = make(<binding-name>,
                                source-location: new-slot.source-location,
                                binding: parsed.token-slot-setter,
                                within: context.context-name);
         new-slot.setter := make-setter(context, new-slot, setter-name, class-type,
                                        sealed: sealed-slot?, markup-token: parsed.slot-doc);
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
    class-type :: <type-fragment>, #rest keys, #key sealed, markup-token)
=> (getter :: <generic-binding>)
   when (markup-token)
      // Can't document virtual slot, because no place to attach markup content
      // except the generic itself, which is not strictly correct.
      doc-comment-on-virtual-slot(location: markup-token.token-src-loc)
   end when;
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   let value = make(<req-value>, name: "value", type: slot.type);
   apply(make-empty-generic, context, name, vector(inst-param), vector(value),
         source-location:, slot.source-location, provenance:, #"declaration", keys)
end method;


define method make-getter
   (context :: <context>, slot :: <inherited-slot>, name :: <binding-name>,
    class-type :: <type-fragment>, #rest keys, #key sealed, markup-token)
=> (getter :: <generic-binding>)
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   apply(make-empty-generic, context, name, vector(inst-param), #f,
         source-location:, slot.source-location, provenance:, #"declaration", keys)
end method;


define method make-getter
   (context :: <context>, slot :: <accessor-slot>, name :: <binding-name>,
    class-type :: <type-fragment>, #rest keys, #key sealed, markup-token)
=> (getter :: <generic-binding>)
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   let value = make(<req-value>, name: "value", type: slot.type);
   apply(make-fixed-method, context, name, vector(inst-param), vector(value),
         source-location:, slot.source-location, provenance:, #"declaration", keys)
end method;


define method make-setter
   (context :: <context>, slot :: <virtual-slot>, name :: <binding-name>,
    class-type :: <type-fragment>, #rest keys, #key sealed, markup-token)
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
    class-type :: <type-fragment>, #rest keys, #key sealed, markup-token)
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
// Merging and replacing
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


define method replace-in-binding (context :: <context>, bind :: <class-binding>)
   let class-defn = bind.explicit-defn;
   if (class-defn)
      for (slot-defn :: <slot> in class-defn.slots)
         // Replace slot getters.
         let repl = element(context.replacements, slot-defn.getter, default: #f);
         if (repl)
            slot-defn.getter := repl;
         end if;
         
         // Replace slot setters.
         if (instance?(slot-defn, <accessor-slot>) & slot-defn.setter)
            let repl = element(context.replacements, slot-defn.setter, default: #f);
            if (repl)
               slot-defn.setter := repl;
            end if;
         end if;
      end for
   end if
end method;


//
// Class inheritance and post-processing
//


define method class-inheritance-graph (bindings :: <sequence>)
=> (inheritance-graph :: <graph>)
   local method superclass-node? (node :: <node>, subclass :: <class-binding>)
         => (superclass? :: <boolean>)
            let super-names = subclass.explicit-defn.direct-supers.simple-names;
            let superclass :: <class-binding> = node.value;
            any?(rcurry(member?, superclass.aliases, test:, \=), super-names)
         end method;
         
   let class-bindings = choose(rcurry(instance?, <class-binding>), bindings);
   let graph = make(<graph>);
   do(curry(make, <node>, graph:, graph, value:), class-bindings);

   for (subclass-node in graph.nodes)
      let subclass = subclass-node.value;
      if (subclass.explicit-defn)
         // Find classes that are superclasses of this class. 
         let superclass-nodes = choose-nodes(graph, rcurry(superclass-node?, subclass));
         let superclasses = map(value, superclass-nodes);
      
         // Connect this class to those classes.
         do(rcurry(connect, subclass-node), superclass-nodes);
         subclass.effective-supers :=
               remove-duplicates!(concatenate!(subclass.effective-supers, superclasses));
         for (superclass in superclasses)
            superclass.effective-subs := add!(superclass.effective-subs, subclass)
         end for
      end if
   end for;

   graph
end method;


define method inherit-slots (apis :: <sequence>, inheritance :: <graph>)
=> ()
   // Order classes by inheritance, deepest last. Determine getters for each.
   let sorted-classes = sorted-graph(inheritance);
   for (class-node in sorted-classes)
      let class-binding = class-node.value;
      let all-getters = make(<stretchy-vector>);
      
      // First, from superclasses.
      let class-supers = map(compose(value, edge-source), class-node.incoming-edges);
      for (class-super :: <class-binding> in class-supers)
         let super-getters = class-super.effective-slots;
         all-getters := concatenate!(all-getters, super-getters);
      end for;
      
      // Second, from self.
      if (class-binding.explicit-defn)
         let self-getters = map(getter, class-binding.explicit-defn.slots);
         all-getters := concatenate!(all-getters, self-getters);
      end if;
      
      class-binding.effective-slots := remove-duplicates!(all-getters);
   end for
end method;


define method inherit-init-args (apis :: <sequence>, inheritance :: <graph>)
=> ()
   let make-binding :: <generic-binding> = $dylan-module.definitions["Make"];
   let initialize-binding :: <generic-binding> = $dylan-module.definitions["Initialize"];
   
   local method same-keyword? (init1 :: <init-arg>, init2 :: <init-arg>)
         => (same? :: <boolean>)
            case-insensitive-equal?(init1.symbol, init2.symbol)
         end method;
   
   // Order classes by inheritance, deepest last. Determine init-args for each.
   let sorted-classes = sorted-graph(inheritance);
   for (class-node in sorted-classes)
      let class-binding = class-node.value;
      let all-keywords = make(<stretchy-vector>);

      // First, from "make".
      let applicable-make :: false-or(<implicit-generic-defn>)
            = applicable-method(make-binding, class-binding);
      if (applicable-make)
         let keywords = keywords-as-init-args(applicable-make);
         all-keywords := concatenate!(all-keywords, keywords);
      end if;
      
      // Second, from class itself. If the class has multiple init args (from
      // several slot declarations), don't assign a type or init spec.
      if (class-binding.explicit-defn)
         let grouped-keywords = group-elements(class-binding.explicit-defn.init-args,
               test: same-keyword?);
         let class-keywords = map(
               method (init-args :: <sequence>) => (init-arg :: <init-arg>)
                  if (init-args.size = 1)
                     init-args.first
                  else
                     make(<init-arg>, symbol: init-args.first.symbol,
                          source-location: init-args.first.source-location)
                  end if
               end method,
               grouped-keywords);
         all-keywords := concatenate!(all-keywords, class-keywords);
      end if;
      
      // Third, from superclasses.
      let class-supers = map(compose(value, edge-source), class-node.incoming-edges);
      for (class-super in class-supers)
         let keywords = class-super.effective-init-args;
         all-keywords := concatenate!(all-keywords, keywords);
      end for;
      
      // Fourth, from "initialize". TODO: This should search up the inheritance
      // hierarchy, since "initialize" should always call "next-method".
      let applicable-initialize :: false-or(<implicit-generic-defn>)
            = applicable-method(initialize-binding, class-binding);
      if (applicable-initialize)
         let keywords = keywords-as-init-args(applicable-initialize);
         all-keywords := concatenate!(all-keywords, keywords);
      end if;
      
      // Merge duplicate init-args, keeping type and init-spec from first of above.
      let grouped-keywords = group-elements(all-keywords, test: same-keyword?);
      class-binding.effective-init-args :=
            map(curry(reduce1, merge-init-args), grouped-keywords);
   end for;
end method;


define method keywords-as-init-args (meth :: <implicit-generic-defn>)
=> (keywords :: <sequence>)
   if (instance?(meth.param-list, <key-param-list>))
      map(as-init-arg, meth.param-list.key-params)
   else
      #[]
   end if
end method;


define method as-init-arg (key :: <key-param>) => (init-arg :: <init-arg>)
   let arg = make(<init-arg>, source-location: key.source-location, symbol: key.symbol);
   arg.type := key.type;
   arg.init-spec := key.expr;
   arg
end method;


define method merge-init-args (init1 :: <init-arg>, init2 :: <init-arg>)
=> (init-arg :: <init-arg>)
   let new-init = make(<init-arg>, source-location: init1.source-location,
         symbol: init1.symbol);
   new-init.type := init1.type | init2.type;
   new-init.init-spec := init1.init-spec | init2.init-spec;
   new-init
end method;


define method applicable-method (gen :: <generic-binding>, cls :: <class-binding>)
=> (defn :: false-or(<implicit-generic-defn>))
   block (found)
      for (defn :: <implicit-generic-defn> in gen.implicit-defns)
         let first-param = element(defn.param-list.req-params, 0, default: #f);
         if (first-param)
            let first-type :: <fragment> = first-param.type;
            if (instance?(first-type, <singleton-type-fragment>))
               first-type := first-type.singleton-expr;
            end if;
            
            let first-name = first-type.simple-name? & first-type.fragment-names.first;
            if (first-name & member?(first-name, cls.aliases, test: \=))
               found(defn)
            end if
         end if
      end for
   end block
end method;
