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
      // Assume that it is a class name, or unparsable.
      if (~supertype.simple-name?)
         unparsable-expression-in-code(location: supertype.source-location)
      else
         bindings := add!(bindings, make-expression-binding
               (context, supertype.source-text.first, type: <class-binding>));
      end if
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


/// Synopsis: Computes the class list sorted by inheritance depth, as well as
/// each class's inheritance information.
define method class-inheritance-list (bindings :: <sequence>)
=> (class-list :: <sequence>)
   let class-bindings = choose(rcurry(instance?, <class-binding>), bindings);
   let graph = make(<graph>);
   let class-table = make(<equal-table>);
   for (binding :: <class-binding> in class-bindings)
      make(<node>, graph: graph, value: binding);
      do(curry(element-setter, binding, class-table), binding.aliases);
   end for;

   for (subclass-node in graph.nodes)
      let subclass = subclass-node.value;
      if (subclass.explicit-defn)
         // Find classes that are superclasses of this class. 
         let superclass-names = map(compose(first, source-text),
               subclass.explicit-defn.direct-supers);
         let superclass-names = remove-duplicates!(superclass-names, test: \=);
         let superclasses = map(curry(element, class-table), superclass-names);
         let superclass-nodes = choose-nodes(graph,
               method (node :: <node>) => (superclass-node? :: <boolean>)
                  member?(node.value, superclasses)
               end);
      
         // Connect this class to those classes.
         do(rcurry(connect, subclass-node), superclass-nodes);
         subclass.effective-supers := superclasses;
         for (superclass in superclasses)
            superclass.effective-subs := add-new!(superclass.effective-subs, subclass)
         end for
      end if
   end for;
   
   if (~graph.acyclic?)
      visit-cycles(graph, inheritance-cycle-error)
   end if;
   
   let sorted-classes = map(value, sorted-graph(graph));
   for (binding :: <class-binding> in sorted-classes)
      binding.effective-cpl := compute-class-linearization(binding);
   end for;

   sorted-classes
end method;


/// Adapted from DRM page 55.
define function compute-class-linearization (c :: <class-binding>) => (cpl :: <list>)
   let inconsistent-cpl? = #f;
   local method merge-lists (reversed-partial-result :: <list>, remaining-inputs :: <sequence>)
            if (every?(empty?, remaining-inputs))
               reverse!(reversed-partial-result)
            else
               local method candidate (c :: <class-binding>)
                        // returns c if it can go in the result now,
                        // otherwise false
                        local method head? (l :: <list>)
                                 c == head(l)
                              end method head?,
                              method tail? (l :: <list>)
                                 member?(c, tail(l))
                              end method tail?;
                        any?(head?, remaining-inputs) & ~any?(tail?, remaining-inputs) & c
                     end method candidate,

                     method candidate-direct-superclass (c :: <class-binding>)
                        any?(candidate, c.effective-supers)
                     end method candidate-direct-superclass;
         
               let next = any?(candidate-direct-superclass, reversed-partial-result);
               if (next)
                  local method remove-next (l :: <list>)
                           if (head(l) == next) tail(l) else l end
                        end method remove-next;
                  merge-lists(pair(next, reversed-partial-result),
                  map(remove-next, remaining-inputs))
               else
                  // In case of an inconsistent graph, just return what we've got.
                  inconsistent-cpl? := #t;
                  reverse!(reversed-partial-result)
               end if
            end if
         end method merge-lists;

   let c-direct-superclasses = c.effective-supers;

   let result = merge-lists(list(c),
         add(map(effective-cpl, c-direct-superclasses),
             as(<list>, c-direct-superclasses)));
   if (inconsistent-cpl?)
      inconsistent-cpl(location: c.source-location)
   end if;
   result
end function;


define function inheritance-cycle-error (cycle :: <sequence>)
   let classes = map(value, cycle);
   circular-class-inheritance(location: classes.first.source-location,
         defn-locations: map(source-location, classes).item-string-list);
end function;


define method inherit-slots (apis :: <sequence>, sorted-classes :: <sequence>)
=> ()
   // Determine getters for each class, deepest inheritance last.
   for (class-binding :: <class-binding> in sorted-classes)
      let all-getters = make(<stretchy-vector>);
      
      // First, from superclasses.
      for (class-super :: <class-binding> in class-binding.effective-supers)
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


define method inherit-init-args (apis :: <sequence>, sorted-classes :: <sequence>)
=> ()
   let make-binding :: <generic-binding> = $dylan-module.definitions["Make"];
   let initialize-binding :: <generic-binding> = $dylan-module.definitions["Initialize"];
   
   local method same-keyword? (init1 :: <init-arg>, init2 :: <init-arg>)
         => (same? :: <boolean>)
            case-insensitive-equal?(init1.symbol, init2.symbol)
         end method;
   
   // Determine init-args for each class, deepest inheritance last.
   for (class-binding :: <class-binding> in sorted-classes)
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
      for (class-super in class-binding.effective-supers)
         let keywords = class-super.effective-init-args;
         all-keywords := concatenate!(all-keywords, keywords);
      end for;
      
      // Fourth, from "initialize" and superclasses' "initialize".
      let applicable-initialize-list
            = applicable-method-list(initialize-binding, class-binding);
      for (applicable-initialize in applicable-initialize-list)
         let keywords = keywords-as-init-args(applicable-initialize);
         all-keywords := concatenate!(all-keywords, keywords);
      end for;
      
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
   new-init.markup-tokens :=
         if (init1.markup-tokens.empty?)
            init2.markup-tokens
         else
            init1.markup-tokens
         end if;
   new-init
end method;


/// Synopsis: Compute functions on and returning each class.
define method note-class-functions (apis :: <sequence>, sorted-classes :: <sequence>)
=> ()
   let functions = choose(rcurry(instance?, type-union(<generic-binding>, <function-binding>)),
                          apis);
   for (class-binding :: <class-binding> in sorted-classes)
      for (func :: type-union(<generic-binding>, <function-binding>) in functions)
         let added-func-on? = #f;
         let added-func-returning? = #f;
         for (defn in func.all-defns, until: added-func-on? & added-func-returning?)
            if (~added-func-on? & func-defn-applicable?(defn, class-binding))
               class-binding.functions-on-class :=
                     add!(class-binding.functions-on-class, func);
               added-func-on? := #t;
            end if;
            if (~added-func-returning? & func-defn-returns?(defn, class-binding))
               class-binding.functions-returning-class :=
                     add!(class-binding.functions-returning-class, func);
               added-func-returning? := #t;
            end if
         end for
      end for
   end for
end method;


define method applicable-method-list (gen :: <generic-binding>, cls :: <class-binding>)
=> (methods :: <sequence>)
   let dylan-object = $dylan-module.definitions["<Object>"];
   let methods = make(<stretchy-vector>);
   iterate next-class (rest = cls.effective-cpl)
      unless (rest.empty?)
         let meth = applicable-method(gen, rest.head);
         if (meth)
            methods := add!(methods, meth);
            next-class(rest.tail);
         elseif (rest.head ~= dylan-object)
            next-class(list(dylan-object))
         end if;
      end unless;
   end iterate;
   methods
end method;


define method applicable-method (gen :: <generic-binding>, cls :: <class-binding>)
=> (defn :: false-or(<implicit-generic-defn>))
   block (found)
      for (defn :: <implicit-generic-defn> in gen.implicit-defns)
         if (func-defn-applicable?(defn, cls))
            found(defn)
         end if
      end for
   end block
end method;


define method func-defn-applicable?
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
                       <explicit-function-defn>),
    cls :: <class-binding>)
=> (applicable? :: <boolean>)
   block (found)
      for (param :: <req-param> in defn.param-list.req-params)
         let type :: false-or(<fragment>) = param.type;
         if (instance?(type, <singleton-type-fragment>))
            type := type.singleton-expr;
         end if;
         let name = type & type.simple-name? & type.fragment-names.first;
         if (name & member?(name, cls.aliases, test: \=))
            found(#t)
         end if
      end for
   end block
end method;


define method func-defn-returns?
   (defn :: type-union(<explicit-generic-defn>, <implicit-generic-defn>,
                       <explicit-function-defn>),
    cls :: <class-binding>)
=> (returns? :: <boolean>)
   block (found)
      let vals = defn.value-list.req-values;
      if (defn.value-list.rest-value)
         vals := add(vals, defn.value-list.rest-value);
      end if;
      for (val :: <value> in vals)
         let type :: false-or(<type-fragment>) = val.type;
         if (instance?(type, <singleton-type-fragment>))
            type := type.singleton-expr;
         end if;
         let name = type & type.simple-name? & type.fragment-names.first;
         if (name & member?(name, cls.aliases, test: \=))
            found(#t)
         end if
      end for
   end block
end method;
