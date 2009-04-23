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
      def1.implicit-defns := remove-duplicates!
            (concatenate!(def1.implicit-defns, def2.implicit-defns));
      def1.sealed-domains := remove-duplicates!
            (concatenate!(def1.sealed-domains, def2.sealed-domains), test: \=);
      def1.sealed? := def1.sealed? & def2.sealed?;
   end unless;
   def1
end method;


//
// Definition creation
//


/**
Synopsis: Create definitions and accessory definitions from a definer macro.

--- Values: ---

defns/tokens-by-name -
   A <table> containing definitions and tokens for a binding name. Elements are
   keyed by <string>; this is the binding name. Each element is a sequence of
   <explicit-X-defn> and <implicit-X-defn> and <token>, <class-slot>, etc.
   instances associated with that binding name.
   
   There will always be at least one element in the table, which will be the
   class, function, etc. defined by 'token'. There may be additional elements
   for slot accessors created by a class definition.

name-tokens -
   A sequence of <source-name>. These are names mentioned in expressions and 
   are assumed to exist as module bindings.
**/
define generic make-definitions (token :: <definition-token>)
=> (defns/tokens-by-name :: <table>, name-tokens :: <sequence>);


define method make-definitions (token :: <constant-definer-token>)
=> (defns/tokens-by-name :: <table>, name-tokens :: <sequence>)
   make-const/var-definition(token, <explicit-constant-defn>, <constant-defn>)
end method;


define method make-definitions (token :: <variable-definer-token>)
=> (defns/tokens-by-name :: <table>, name-tokens :: <sequence>)
   make-const/var-definition(token, <explicit-variable-defn>, <variable-defn>)
end method;


define method make-const/var-definition
   (token :: type-union(<constant-definer-token>, <variable-definer-token>),
    expl-defn-class :: <class>, defn-class :: <class>)
=> (defns/tokens-by-name :: <table>, name-tokens :: <sequence>)
   let def/tok-table = make(<case-insensitive-string-table>);
   let expl-def = make(expl-defn-class, source-location: token.token-src-loc,
                       type: token.api-type & as(<type-fragment>, token.api-type),
                       value: as(<computed-constant>, token.api-value));
   let name-seq = fragment-names(expl-def.value);
   when (expl-def.type)
      name-seq := concatenate!(name-seq, fragment-names(expl-def.type))
   end when;
   def/tok-table[token.api-name] :=
         vector(make(defn-class, explicit: expl-def), token);
   values(def/tok-table, name-seq)
end method;


define method make-definitions (token :: <macro-definer-token>)
=> (defns/tokens-by-name :: <table>, name-tokens :: <sequence>)
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
   let expl-defn = make(defn-class, source-location: token.token-src-loc);
   let defn = make(<macro-defn>, explicit: expl-defn);
   values(table(<case-insensitive-string-table>, token.api-name => vector(defn, token)),
          #[])
end method;


//
// Utility functions
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


define method as (cls == <vendor-option>, tok :: <property-token>)
=> (opt :: <vendor-option>)
   make(<vendor-option>, symbol: tok.prop-name,
        fragment: as(<code-fragment>, tok.prop-value))
end method;


define method type-from-name (name :: <string>, loc :: <source-location>)
=> (frag :: <type-fragment>)
   let source-name = make(<source-name>, name: name, source-location: loc);
   make(<type-fragment>, text: vector(source-name), source-location: loc);
end method;


define constant $object-type =
   type-from-name("<object>", make(<unknown-source-location>));


define method \= (frag1 :: <type-fragment>, frag2 :: <type-fragment>)
=> (equal? :: <boolean>)
   
end method;


define method singleton-from-source-text
   (text :: <text-token>, loc :: <source-location>)
=> (frag :: <type-fragment>)
   let singleton-name = make(<source-name>, name: "singleton", source-location: loc);
   let operand-text = as(<computed-constant>, text).source-text;
   let full-text = concatenate(vector(singleton-name, '('), operand-text, vector(')'));
   make(<type-fragment>, text: full-text, source-location: loc)
end method;
