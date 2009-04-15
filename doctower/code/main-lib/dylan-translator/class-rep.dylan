module: dylan-translator


define method make-expl/impl-defn (token :: <class-definer-token>)
=> (class-def :: <explicit-class-defn>, name-tokens :: <sequence>)
   let class-def = make(<explicit-class-defn>, source-location: token.token-src-loc);
   let names = make(<stretchy-vector>);
   
   map-into(class-def.adjs, curry(as, <symbol>), token.api-modifiers);
   class-def.adjs.remove-duplicates!;

   map-into(class-def.direct-supers, curry(as, <type-fragment>), token.class-supers);
   for (superclass :: <type-fragment> in class-def.direct-supers)
      names := concatenate!(names, superclass.fragment-names)
   end for;
   
   // map-into(class-def.slots, make-slot, token.class-slots);
   // map-into(class-def.init-args, make-init-arg, concatenate())

   values(class-def, names);
end method;



