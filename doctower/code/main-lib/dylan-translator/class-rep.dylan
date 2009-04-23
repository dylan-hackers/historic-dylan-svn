module: dylan-translator


define method make-definitions (token :: <class-definer-token>)
=> (defn/tokens-by-name :: <table>, name-tokens :: <sequence>)
   let def/tok-table = make(<case-insensitive-string-table>);
   let name-seq = make(<stretchy-vector>);
   let class-def = make(<explicit-class-defn>, source-location: token.token-src-loc);

   // Adjectives
   map-into(class-def.adjs, curry(as, <symbol>), token.api-modifiers);
   unless (member?(#"open", class-def.adjs))
      add!(class-def.adjs, #"sealed");
   end unless;
   unless (member?(#"primary", class-def.adjs))
      add!(class-def.adjs, #"free");
   end unless;
   unless (member?(#"abstract", class-def.adjs))
      add!(class-def.adjs, #"concrete");
   end unless;
   class-def.adjs.remove-duplicates!;

   // Superclasses
   map-into(class-def.direct-supers, curry(as, <type-fragment>), token.class-supers);
   for (superclass :: <type-fragment> in class-def.direct-supers)
      name-seq := concatenate!(name-seq, superclass.fragment-names)
   end for;
   
   // Slots
   let class-type = type-from-name(token.api-name, class-def.source-location);
   map-into(class-def.slots,
         method (parsed-slot :: <parsed-class-slot>) => (slot :: <slot>)
            let (slot, expr-names) = make-slot(parsed-slot, class-type);
            name-seq := concatenate!(name-seq, expr-names);
            if (instance?(slot, <accessor-slot>))
               def/tok-table[parsed-slot.slot-name] :=
                     vector(slot.getter, parsed-slot);
               if (parsed-slot.slot-setter)
                  def/tok-table[parsed-slot.slot-setter] :=
                        vector(slot.setter, parsed-slot);
               end if;
            end if;
            slot
         end method, token.class-slots);
         
   // Init args
   map-into(class-def.init-args,
         method (parsed-keyword :: <class-keyword>) => (init-arg :: <init-arg>)
            let (init-arg, expr-names) = make-init-arg(parsed-keyword);
            name-seq := concatenate!(name-seq, expr-names);
            init-arg
         end method, token.class-keywords);

   // Return value
   def/tok-table[token.api-name] :=
         vector(make(<class-defn>, explicit: class-def), token);
   values(def/tok-table, name-seq);
end method;


define method make-slot
   (parsed :: <parsed-class-slot>, class-type :: <type-fragment>)
=> (slot :: <slot>, expr-names :: <sequence>)
   let expr-names = #[];
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
      end select;
   end for;
   let new-slot = make(slot-class, source-location: parsed.token-src-loc);

   when (instance?(new-slot, <initable-slot>) & parsed.slot-init)
      let (spec, names) = init-spec-from-text(parsed.slot-init, expr-names);
      new-slot.init-spec := spec;
      expr-names := concatenate!(expr-names, names);
   end when;
   
   when (instance?(new-slot, <accessor-slot>))
      // Prepare to make getter and setter.
      new-slot.sealed? := sealed-slot?;
      when (parsed.slot-type)
         new-slot.type := as(<type-fragment>, parsed.slot-type);
         expr-names := concatenate!(expr-names, fragment-names(new-slot.type))
      end when;

      // Make getter and setter.
      new-slot.getter := make-getter-definition(new-slot, class-type);
      if (parsed.slot-setter)
         new-slot.setter := make-setter-definition(new-slot, class-type);
      end if;
   end when;
   
   values(new-slot, expr-names)
end method;


define method make-init-arg (parsed :: <class-keyword>)
=> (init-arg :: <init-arg>, expr-names :: <sequence>)
   let expr-names = #[];
   let new-init-arg = make(<init-arg>, symbol: parsed.keyword-name);

   when (parsed.keyword-type)
      let type-frag = as(<type-fragment>, parsed.keyword-type);
      new-init-arg.type := type-frag;
      expr-names := concatenate!(expr-names, fragment-names(type-frag));
   end when;

   when (parsed.keyword-init)
      let (spec, names) = init-spec-from-text(parsed.keyword-init, expr-names);
      new-init-arg.init-spec := spec;
      expr-names := concatenate!(expr-names, names);
   end when;
   
   values(new-init-arg, expr-names)
end method;


define method init-spec-from-text
   (token :: <text-token>, names :: <sequence>)
=> (init-spec :: <init-spec>, names :: <sequence>)
   let frag = as(<code-fragment>, token);
   values(make(<init-spec>, fragment: frag),
          concatenate(names, frag.fragment-names))
end method;


define method make-getter-definition
   (slot :: <virtual-slot>, class-type :: <type-fragment>)
=> (getter :: <generic-defn>)
   let generic = make(<generic-defn>, explicit: #f, implicit: #[]);
   if (slot.sealed?)
      seal-generic(generic, class-type)
   end if;
   generic
end method;


define method make-getter-definition
   (slot :: <slot>, class-type :: <type-fragment>)
=> (getter :: <generic-defn>)
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   let value = make(<req-value>, name: "value", type: slot.type);
   let meth = make-fixed-method(vector(inst-param), vector(value),
                                slot.source-location);
   let generic = make(<generic-defn>, explicit: #f, implicit: vector(meth));
   if (slot.sealed?)
      seal-generic(generic, class-type)
   end if;
   generic
end method;


define method make-setter-definition
   (slot :: <virtual-slot>, class-type :: <type-fragment>)
=> (setter :: <generic-defn>)
   let generic = make(<generic-defn>, explicit: #f, implicit: #[]);
   if (slot.sealed?)
      seal-generic(generic, slot.type, class-type)
   end if;
   generic
end method;


define method make-setter-definition
   (slot :: <slot>, class-type :: <type-fragment>)
=> (setter :: <generic-defn>)
   let newval-param = make(<req-param>, name: "new-value", type: slot.type);
   let inst-param = make(<req-param>, name: "instance", type: class-type);
   let value = make(<req-value>, name: "new-value", type: slot.type);
   let meth = make-fixed-method(vector(newval-param, inst-param), vector(value),
                                slot.source-location);
   let generic = make(<generic-defn>, explicit: #f, implicit: vector(meth));
   if (slot.sealed?)
      seal-generic(generic, slot.type, class-type)
   end if;
   generic
end method;
