module: dylan-parser
synopsis: Functions and classes used in various places.

//
// General helpers
//

define method remove-from-outer-scope (context, #rest items)
=> ()
   unless (items.empty?)
      let outer-scope = attr(scoped-docs);
      for (item in items)
         if (item) remove!(outer-scope, item, test: \=) end;
      end for;
   end unless;
end method;

define method add-to-front (item, seq :: <sequence>) => (seq :: <sequence>)
   if (item) concatenate-as(seq.type-for-copy, vector(item), seq) else seq end
end method;

define method skipped? (item) => (skipped? :: <boolean>)
   instance?(item, <skipped-token>)
end method;

define method choose-doc-comments (seq) => (seq :: <sequence>)
   choose(rcurry(instance?, <doc-comment-token>), seq);
end method;

define method choose-unskipped (seq) => (seq :: <sequence>)
   choose(complement(skipped?), seq)
end method;

/// Used to make a list of x from tokens parsed using (x? (s x)* s?).
define method list-from-tokens (tokens) => (seq :: <sequence>)
   add-to-front(tokens[0], collect-subelements(tokens[1], 1) | #[])
end method;

define method \= (doc1 :: <doc-comment-token>, doc2 :: <doc-comment-token>)
=> (equal? :: <boolean>)
   (doc1 == doc2) |
   (doc1.parse-start = doc2.parse-start) & (doc1.parse-end = doc2.parse-end)
end method;


//
// Source records
//

// Structure is [] or [[lines-til-parsable/#f, definition/doc-block], ...]
define method source-record-definitions (meat) => (defns)
   let flat-list = collect-subelements(meat, 1);
   choose(rcurry(instance?, <definition-token>), flat-list);
end method;


//
// Slots and initialization keywords
//

define class <class-keyword> (<object>)
   slot keyword-from-slot? :: <boolean>;
   slot keyword-required? :: <boolean>;
   slot keyword-name :: <string>;
   slot keyword-type :: false-or(<text-token>);
   slot keyword-init :: false-or(<text-token>);
   slot keyword-doc :: false-or(<doc-comment-token>);
end class;

define class <class-slot> (<object>)
   slot slot-modifiers :: <sequence> /* of <string> */;
   slot slot-name :: <string>;
   slot slot-type :: false-or(<text-token>);
   slot slot-init :: false-or(<text-token>);
   slot slot-setter :: false-or(<string>);
   slot slot-doc :: false-or(<doc-comment-token>);
end class;

define method slot-from-clause (tok :: <token>)
=> (slot :: singleton(#f))
   #f
end method;

define method slot-from-clause (tok :: <slot-spec-token>)
=> (slot :: <class-slot>)
   let slot = make(<class-slot>);
   slot.slot-doc := tok.clause-doc;
   slot.slot-modifiers := tok.slot-modifiers;
   slot.slot-name := tok.slot-name;
   slot.slot-type := tok.slot-type | clauses-type-option(tok.clause-options);
   slot.slot-init := tok.init-expression | clauses-init-option(tok.clause-options);

   let const? = member?("constant", slot.slot-modifiers, test: case-insensitive-equal?);
   let (setter-present?, setter-name) = clauses-setter-option(tok.clause-options);
   slot.slot-setter :=
         case
            const? => #f;
            setter-present? & setter-name => setter-name;
            setter-present? & ~setter-name =>
               slot.slot-modifiers := add!(slot.slot-modifiers, "constant");
               #f;
            otherwise => concatenate(slot.slot-name, "-setter");
         end case;

   slot;
end method;

define method keyword-from-clause (tok :: <token>)
=> (keyword :: singleton(#f))
   #f
end method;

define method keyword-from-clause (tok :: <slot-spec-token>)
=> (keyword :: false-or(<class-keyword>))
   let keyword-option = clauses-keyword-option(tok.clause-options);
   if (keyword-option)
      let keyword = make(<class-keyword>);
      keyword.keyword-from-slot? := #t;
      keyword.keyword-required? :=
            instance?(keyword-option, <required-init-keyword-option-token>);
      keyword.keyword-name := keyword-option.value;
      keyword.keyword-type := tok.slot-type | clauses-type-option(tok.clause-options);
      keyword.keyword-init := tok.init-expression | clauses-init-option(tok.clause-options);
      keyword.keyword-doc := tok.clause-doc;
      keyword;
   end if;
end method;

define method keyword-from-clause (tok :: <init-arg-spec-token>)
=> (keyword :: <class-keyword>)
   let keyword = make(<class-keyword>);
   keyword.keyword-from-slot? := #f;
   keyword.keyword-required? := tok.keyword-required?;
   keyword.keyword-name := tok.keyword-name;
   keyword.keyword-type := clauses-type-option(tok.clause-options);
   keyword.keyword-init := clauses-init-option(tok.clause-options);
   keyword.keyword-doc := tok.clause-doc;
   keyword;
end method;

define method clauses-init-option (seq) => (tok :: false-or(<text-token>))
   let <init-option-type> =
         type-union(<init-value-option-token>, <init-function-option-token>);
   let tok = find-element(seq, rcurry(instance?, <init-option-type>), failure: #f);
   tok & tok.value
end method;

define method clauses-type-option (seq) => (tok :: false-or(<text-token>))
   let tok = find-element(seq, rcurry(instance?, <type-option-token>), failure: #f);
   tok & tok.value
end method;

define method clauses-setter-option (seq)
=> (present? :: <boolean>, setter :: false-or(<string>))
   let tok = find-element(seq, rcurry(instance?, <setter-option-token>), failure: #f);
   values(tok.true?, tok & tok.value);
end method;

define method clauses-keyword-option (seq) => (tok :: false-or(<token>))
   let <keyword-option-type> =
         type-union(<init-keyword-option-token>, <required-init-keyword-option-token>);
   find-element(seq, rcurry(instance?, <keyword-option-type>), failure: #f);
end method;

/// A keyword may be defined in a slot clause and also in a keyword clause. Keep
/// only the keyword clause. Otherwise, allow duplicate keywords.
define method remove-duplicate-keywords (seq) => (seq)
   let new-seq = make(<stretchy-vector>);
   for (k1 from 0 below seq.size)
      block (skip)
         // We only need to test against other keywords if defined in slot clause.
         // If it is defined in keyword clause, we always keep it.
         when (seq[k1].keyword-from-slot?)
            for (k2 from k1 + 1 below seq.size)
               if (case-insensitive-equal?(seq[k1].keyword-name, seq[k2].keyword-name))
                  // If any of the keywords we check against are defined by a
                  // keyword clause, skip the current slot-clause-defined keyword.
                  when (~seq[k2].keyword-from-slot?)
                     skip();
                  end when;
               end if;
            end for;
         end when;
         add!(new-seq, seq[k1]);
      end block;
   end for;
   new-seq
end method;


//
// Parameter lists
//

define abstract class <func-param> (<object>)
   slot param-doc :: false-or(<doc-comment-token>), init-keyword: #"doc";
end class;

define abstract class <func-argument> (<func-param>)
end class;

define abstract class <func-value> (<func-param>)
end class;

define abstract class <required-argument> (<func-argument>)
   slot param-name :: <string>;
end class;

define class <required-typed-argument> (<required-argument>)
   slot param-type :: false-or(<text-token>), init-keyword: #"type";
end class;

define class <required-singleton-argument> (<required-argument>)
   slot param-instance :: false-or(<text-token>), init-keyword: #"instance";
end class;

define class <rest-argument> (<func-argument>)
   slot param-name :: <string>, init-keyword: #"name";
end class;

define class <keyword-argument> (<func-argument>)
   /// The keyword symbol; the actual variable name is of less interest.
   slot param-name :: <string>;
   slot param-type :: false-or(<text-token>);
   slot param-default :: false-or(<text-token>);
end class;

define class <all-keys-argument> (<func-argument>)
end class;

define class <required-value> (<func-value>)
   slot param-name :: <string>, init-keyword: #"name";
   slot param-type :: false-or(<text-token>), init-keyword: #"type";
end class;

define class <rest-value> (<func-value>)
   slot param-name :: <string>, init-keyword: #"name";
   slot param-type :: false-or(<text-token>), init-keyword: #"name";
end class;

define method parameter-list-from-token (tok == #f)
=> (param-list :: <sequence>)
   #[]
end method;

define method parameter-list-from-token (params-tok :: <parameters-token>)
=> (param-list :: <sequence>)
   let required-param-list = map-as(<stretchy-vector>, required-param-from-token,
                                    params-tok.required-params);
   let rest-key-param-list = rest-key-params-from-token(params-tok.rest-key-param);
   concatenate(required-param-list, rest-key-param-list);
end method;

define method required-param-from-token (tok == #f)
=> (param-list :: <sequence>)
   #[]
end method;

define method required-param-from-token (param-tok :: <required-parameter-token>)
=> (param :: <required-argument>)
   let param =
         if (param-tok.req-sing?)
            make(<required-singleton-argument>, instance: param-tok.req-inst);
         else
            make(<required-typed-argument>, type: param-tok.req-var.type);
         end if;
   param.param-doc := param-tok.req-doc;
   param.param-name := param-tok.req-var.name;
   param
end method;

define method rest-key-params-from-token (tok == #f)
=> (param-list :: <sequence>)
   #[]
end method;

define method rest-key-params-from-token (tok :: <rest-key-parameter-list-token>)
=> (param-list :: <sequence>)
   let rest-param = tok.rest-var &
         make(<rest-argument>, doc: tok.rest-doc, name: tok.rest-var.name);
   let key-param-list = key-params-from-token(tok.key-param);
   add-to-front(rest-param, key-param-list);
end method;

define method key-params-from-token (tok == #f)
=> (param-list :: <sequence>)
   #[]
end method;

define method key-params-from-token (tok :: <key-parameter-list-token>)
=> (param-list :: <sequence>)
   let all-keys-param = tok.all-keys? &
         make(<all-keys-argument>, doc: tok.all-keys-doc);
   let key-param-list = map-as(<stretchy-vector>, key-param-from-token,
                               tok.key-params);
   if (all-keys-param)
      add!(key-param-list, all-keys-param);
   else
      key-param-list;
   end if;
end method;

define method key-param-from-token (tok :: <keyword-parameter-token>)
=> (param :: <keyword-argument>)
   let keyword = make(<keyword-argument>, doc: tok.key-doc);
   keyword.param-name := tok.key-symbol | tok.key-var.name;
   keyword.param-type := tok.key-var.type;
   keyword.param-default := tok.key-default;
   keyword
end method;

define method value-list-from-token (token == #f)
=> (value-list :: <sequence>)
   #[]
end method;

define method value-list-from-token (token :: <variable-token>)
=> (value-list :: <sequence>)
   vector(value-from-token(token))
end method;

define method value-list-from-token (token :: <values-list-token>)
=> (value-list :: <sequence>)
   let rest-value = token.rest-val &
         make(<rest-value>, doc: token.rest-doc,
              name: token.rest-val.name, type: token.rest-val.type);
   let req-values = map-as(<stretchy-vector>, value-from-token, token.required-vals);
   if (rest-value)
      add!(req-values, rest-value)
   else
      req-values;
   end if;
end method;

define method value-from-token (token :: <variable-token>)
=> (value :: <required-value>)
   make(<required-value>, doc: token.var-doc, name: token.name, type: token.type);
end method;
