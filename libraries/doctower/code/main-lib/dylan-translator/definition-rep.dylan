module: dylan-translator
synopsis: Common code dealing with definition and fragment representations.


//
// Definition merging
//


define method add-definition! (def1 :: <definition>, def2 == #f)
=> (def :: <definition>)
   def1
end method;


define method add-definition! (def1 :: <definition>, def2 :: <definition>)
=> (def :: <definition>)
   unless (def1 == def2)
      let locs = map(source-location, concatenate(def1.all-defns, def2.all-defns));
      conflicting-definitions-in-code(location: locs.first,
            defn-locations: locs.item-string-list);
   end unless;
   def1
end method;


define method add-definition! (def1 :: <generic-defn>, def2 :: <generic-defn>)
=> (def :: <generic-defn>)
   unless (def1 == def2)
      if (def1.explicit-defn & def2.explicit-defn)
         let locs = vector(def1.explicit-defn.source-location,
                           def2.explicit-defn.source-location);
         conflicting-definitions-in-code(location: locs.first,
               defn-locations: locs.item-string-list);
      end if;
      def1.explicit-defn := def1.explicit-defn | def2.explicit-defn;
      def1.implicit-defns :=
            remove-duplicates!(concatenate!(def1.implicit-defns, def2.implicit-defns));
   end unless;
   def1
end method;


//
// Definition creation
//


define method as (cls :: subclass(<fragment>), tok :: <text-token>)
=> (frag :: <fragment>)
   let frag-text = map-as(<vector>,
         method (tok-elem :: type-union(<character>, <text-name-token>))
         => (frag-elem :: type-union(<character>, <source-name>))
            if (instance?(tok-elem, <text-name-token>))
               make(<source-name>, source-location: tok-elem.token-src-loc,
                    name: tok-elem.api-name)
            else
               tok-elem
            end if
         end, tok.token-text);
   make(cls, text: frag-text, source-location: tok.token-src-loc)
end method;


define method make-expl/impl-defn (token :: <generic-definer-token>)
=> (defn :: <explicit-generic-defn>, name-tokens :: <sequence>)
   values(make(<explicit-generic-defn>, source-location: token.token-src-loc),
          #[]);
end method;


define method make-expl/impl-defn (token :: <method-definer-token>)
=> (defn :: <implicit-generic-defn>, name-tokens :: <sequence>)
   values(make(<implicit-generic-defn>, source-location: token.token-src-loc),
          #[]);
end method;


define method make-expl/impl-defn (token :: <function-definer-token>)
=> (defn :: <explicit-function-defn>, name-tokens :: <sequence>)
   values(make(<explicit-function-defn>, source-location: token.token-src-loc),
          #[]);
end method;


define method make-expl/impl-defn (token :: <constant-definer-token>)
=> (defn :: <explicit-constant-defn>, name-tokens :: <sequence>)
   values(make(<explicit-constant-defn>, source-location: token.token-src-loc),
          #[]);
end method;


define method make-expl/impl-defn (token :: <variable-definer-token>)
=> (defn :: <explicit-variable-defn>, name-tokens :: <sequence>)
   values(make(<explicit-variable-defn>, source-location: token.token-src-loc),
          #[]);
end method;


define method make-expl/impl-defn (token :: <macro-definer-token>)
=> (defn :: <explicit-macro-defn>, name-tokens :: <sequence>)
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
   values(make(defn-class, source-location: token.token-src-loc),
          #[]);
end method;

