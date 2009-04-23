module: dylan-translator


define method make-definitions (token :: <generic-definer-token>)
=> (defn/tokens-by-name :: <table>, name-tokens :: <sequence>)
   let def/tok-table = make(<case-insensitive-string-table>);
   let expl-defn = make(<explicit-generic-defn>, source-location: token.token-src-loc);
   let generic-defn = make(<generic-defn>, explicit: expl-defn);

   // Adjectives
   map-into(expl-defn.adjs, curry(as, <symbol>), token.api-modifiers);
   if (member?(#"open", expl-defn.adjs))
      generic-defn.sealed? := #f;
   else
      add!(expl-defn.adjs, #"sealed");
   end if;
   expl-defn.adjs.remove-duplicates!;
   
   // Parameter list
   let (param-list, param-names) = make-param-list(token.func-params);
   let (value-list, value-names) = make-value-list(token.func-values);
   expl-defn.parameter-list :=
         make(<parameter-list>, param-list: param-list, value-list: value-list);
   
   // Vendor options
   map-into(expl-defn.vendor-options, curry(as, <vendor-option>), token.func-options);

   // Return value
   def/tok-table[token.api-name] := vector(generic-defn, token);
   values(def/tok-table, concatenate(param-names, value-names));
end method;


define method make-definitions (token :: <method-definer-token>)
=> (defn/tokens-by-name :: <table>, name-tokens :: <sequence>)
   let def/tok-table = make(<case-insensitive-string-table>);
   let impl-defn = make(<implicit-generic-defn>, source-location: token.token-src-loc);
   let generic-defn = make(<generic-defn>, implicit: vector(impl-defn));

   // Adjectives
   map-into(impl-defn.adjs, curry(as, <symbol>), token.api-modifiers);
   impl-defn.adjs.remove-duplicates!;

   // Parameter list
   let (param-list, param-names) = make-param-list(token.func-params);
   let (value-list, value-names) = make-value-list(token.func-values);
   impl-defn.parameter-list :=
         make(<parameter-list>, param-list: param-list, value-list: value-list);
   
   // Sealed domain
   if (member?(#"sealed", impl-defn.adjs))
      apply(seal-generic, generic-defn, map(type, param-list.req-params))
   end if;

   // Return value
   def/tok-table[token.api-name] := vector(generic-defn, token);
   values(def/tok-table, concatenate(param-names, value-names));
end method;


define method make-definitions (token :: <function-definer-token>)
=> (defn/tokens-by-name :: <table>, name-tokens :: <sequence>)
   let def/tok-table = make(<case-insensitive-string-table>);
   let expl-defn = make(<explicit-function-defn>, source-location: token.token-src-loc);
   let function-defn = make(<function-defn>, explicit: expl-defn);

   // Adjectives
   map-into(expl-defn.adjs, curry(as, <symbol>), token.api-modifiers);
   expl-defn.adjs.remove-duplicates!;

   // Parameter list
   let (param-list, param-names) = make-param-list(token.func-params);
   let (value-list, value-names) = make-value-list(token.func-values);
   expl-defn.parameter-list :=
         make(<parameter-list>, param-list: param-list, value-list: value-list);
   
   def/tok-table[token.api-name] := vector(function-defn, token);
   values(def/tok-table, concatenate(param-names, value-names));
end method;


define method make-definitions (token :: <domain-definer-token>)
=> (defn/tokens-by-name :: <table>, name-tokens :: <sequence>)
   let generic = make(<generic-defn>, explicit: #f, implicit: #[]);
   let types = map(curry(as, <type-fragment>), token.domain-types);
   let names = apply(concatenate, #[], map(fragment-names, types));
   apply(seal-generic, generic, types);
   let def/tok-table = make(<case-insensitive-string-table>);
   def/tok-table[token.api-name] := vector(generic);
   values(def/tok-table, names)
end method;


define method make-param-list (parsed-params :: <sequence> /* of <func-argument> */)
=> (param-list :: <param-list>, name-tokens :: <sequence>)
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
   let name-seq = make(<stretchy-vector>);
   for (param in parsed-params)
      select (param by instance?)

         <required-typed-argument> =>
            let param-rep = make(<req-param>, name: param.param-name);
            when (param.param-type)
               param-rep.type := as(<type-fragment>, param.param-type);
               name-seq := concatenate!(name-seq, fragment-names(param-rep.type));
            end when;
            param-list.req-params := add!(param-list.req-params, param-rep);

         <required-singleton-argument> =>
            let param-rep = make(<req-param>, name: param.param-name);
            when (param.param-instance)
               param-rep.type := singleton-from-source-text
                     (param.param-instance, param.token-src-loc);
               name-seq := concatenate!(name-seq, fragment-names(param-rep.type));
            end when;
            param-list.req-params := add!(param-list.req-params, param-rep);

         <rest-argument> =>
            let param-rep = make(<rest-param>, name: param.param-name);
            param-list.rest-param := param-rep;

         <keyword-argument> =>
            let param-rep = make(<key-param>, name: param.param-name);
            when (param.param-type)
               param-rep.type := as(<type-fragment>, param.param-type);
               name-seq := concatenate!(name-seq, fragment-names(param-rep.type));
            end when;
            when (param.param-default)
               param-rep.expr := as(<code-fragment>, param.param-default);
               name-seq := concatenate!(name-seq, fragment-names(param-rep.expr));
            end when;
            param-list.key-params := add!(param-list.key-params, param-rep);

         <accepts-keys-argument> =>
            #f; // Do nothing; this only marks #key.

         <all-keys-argument> =>
            param-list.all-keys? := #t;
      end select;
   end for;
   values(param-list, name-seq);
end method;


define method make-value-list (parsed-vals :: <sequence> /* of <func-value> */)
=> (value-list :: <value-list>, name-tokens :: <sequence>)
   let value-list = make(<value-list>);
   let name-seq = make(<stretchy-vector>);
   for (val in parsed-vals)
      select (val by instance?)
         <required-value> =>
            let value-rep = make(<req-value>, name: val.param-name);
            when (val.param-type)
               value-rep.type := as(<type-fragment>, val.param-type);
               name-seq := concatenate!(name-seq, fragment-names(value-rep.type));
            end when;
            value-list.req-values := add!(value-list.req-values, value-rep);
         <parsed-rest-value> =>
            let value-rep = make(<rest-value>, name: val.param-name);
            when (val.param-type)
               value-rep.type := as(<type-fragment>, val.param-type);
               name-seq := concatenate!(name-seq, fragment-names(value-rep.type));
            end when;
            value-list.rest-value := value-rep;
      end select;
   end for;
   values(value-list, name-seq);
end method;


define method make-fixed-method
   (meth-params :: <sequence>, meth-values :: <sequence>, loc :: <source-location>)
=> (meth :: <implicit-generic-defn>)
   let meth-param-list = make(<fixed-param-list>, req-params: meth-params);
   let meth-value-list = make(<value-list>, req-values: meth-values);
   let meth-parameter-list =
         make(<parameter-list>, param-list: meth-param-list, value-list: meth-value-list);
   make(<implicit-generic-defn>, parameter-list: meth-parameter-list,
        source-location: loc)
end method;


define method seal-generic (generic :: <generic-defn>, #rest types)
=> ()
   let types = as(type-for-copy(types), types);
   replace-elements!(types, false?, always($object-type));
   assert(every?(rcurry(instance?, <type-fragment>), types),
          "seal-generic called with illegal arguments");
   let sealed-spec = make(<sealed-domain>, types: types);
   generic.sealed-domains := add-new!(generic.sealed-domains, sealed-spec, test: \=);
end method;
