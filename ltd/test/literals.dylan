//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Literals.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Data Structures
// A literal
define class <literal-node> (<object>)
  slot literal-negated-p, init-keyword: #"literal-negated-p";
  slot literal-relation, init-keyword: #"literal-relation";
  slot literal-terms, init-keyword: #"literal-terms";
end class <literal-node>;

// ----------------------------------------------------------------------------
define method literal-node-print-function (structure, stream, depth)
  format(stream, "<");
  print-literal-node(structure, s: stream);
  format(stream, ">");
end method literal-node-print-function;

define method print-literal-node (node, #key s = #t, flip-negation = #f)
  if (*display-logic-as-lists*)
    print-literal-node-as-list(node, s: s, flip-negation: flip-negation);
  else
    print-literal-node-as-logic(node, s: s, flip-negation: flip-negation);
  end if;
end method print-literal-node;

define method print-literal-node-as-list (node,
                                          #key s = #t,
                                          flip-negation = #f)
  if (~ (node.literal-negated-p == flip-negation)) format(s, "(not "); end if;
  format(s, "(");
  (formatter-1("~:(~A~)"))(s, node.literal-relation);
  if (node.literal-terms)
    (formatter-1("~{ ~S~}"))(s, node.literal-terms);
  end if;
  format(s, ")");
  if (~ (node.literal-negated-p == flip-negation)) format(s, ")"); end if;
end method print-literal-node-as-list;

define method print-literal-node-as-logic (node,
                                           #key s = #t,
                                           flip-negation = #f)
  if (~ (node.literal-negated-p == flip-negation)) format(s, "~"); end if;
  (formatter-1("~:(~A~)"))(s, node.literal-relation);
  if (node.literal-terms)
    let term-strings = #f;
    term-strings
     := map(method (term)
              let s = allocate-resource(#"string-output-simple-stream");
              #"character";
              term-to-string(term, s);
              let _
                  = // LTD: Function GET-OUTPUT-STREAM-STRING not yet implemented.
                    get-output-stream-string(s);
              deallocate-resource(#"string-output-simple-stream", s);
              _;
            end method,
            node.literal-terms);
    (formatter-1("(~A~{,~A~})"))(s, head(term-strings), tail(term-strings));
  end if;
end method print-literal-node-as-logic;

define method term-to-string (term, #key s = #t)
  // Variable terms -> lowercase string, Constant terms -> capitalized string
  if (varp(term))
    (formatter-1("~(~A~)"))(s, variable-to-string(term));
  elseif (instance?(term, <pair>))
    (formatter-1("~:(~A~)"))(s, first(term));
    if (tail(term)) format(s, "("); end if;
    format(s, "%S",
           begin
             let str = allocate-resource(#"string-output-simple-stream");
             #"character";
             for (remaining-terms = tail(term) then tail(remaining-terms),
                  until empty?(remaining-terms),
                  subterm = first(remaining-terms) then first(remaining-terms))
               term-to-string(subterm, str);
               if (tail(remaining-terms)) format(str, ","); end if;
             end for;
             let _
                 = // LTD: Function GET-OUTPUT-STREAM-STRING not yet implemented.
                   get-output-stream-string(str);
             deallocate-resource(#"string-output-simple-stream", str);
             _;
           end);
    if (tail(term)) format(s, ")"); end if;
  elseif (instance?(term, <string>))
    format(s, "%=", term);
  else
    (formatter-1("~:(~A~)"))(s, term);
  end if;
end method term-to-string;

// ----------------------------------------------------------------------------
define method list-to-literal (list, #key replacement-bindings = #f)
  let lit = make-literal-node();
  if (#"not" == first(list))
    lit.literal-negated-p := #t;
    list := second(list);
  end if;
  lit.literal-relation := first(list);
  if (replacement-bindings)
    lit.literal-terms := plug(tail(list), replacement-bindings);
  else
    lit.literal-terms := tail(list);
  end if;
  lit;
end method list-to-literal;

// ----------------------------------------------------------------------------
define method literal-to-list (literal, #key ignore-negation = #f)
  let new-list = pair(literal.literal-relation, literal.literal-terms);
  if (~ ignore-negation)
    if (literal.literal-negated-p) new-list := list(#"not", new-list); end if;
  end if;
  new-list;
end method literal-to-list;

// ----------------------------------------------------------------------------
define method literal-list-equal-p (literal, list, #key test = \=)
  // True iff the literal is equivalent to the list representation
  let list-negation = first(list) == #"not";
  if (list-negation) list := second(list); end if;
  literal.literal-negated-p == list-negation
   & literal.literal-relation == first(list)
   & test(literal.literal-terms, tail(list));
end method literal-list-equal-p;

// ----------------------------------------------------------------------------
define method literal-plug (literal, binding-list)
  // Return a new literal, which is a copy of LITERAL with BINDING-LIST applied
  let copy = copy-literal-node(literal);
  copy.literal-terms := plug(copy.literal-terms, binding-list);
  copy;
end method literal-plug;

define method nliteral-plug (literal, binding-list)
  // Destructively modify LITERAL by applying BINDING-LIST
  literal.literal-terms := plug(literal.literal-terms, binding-list);
  literal;
end method nliteral-plug;

// ----------------------------------------------------------------------------
define method literal-flip-negation (literal)
  // Return a copy of LITERAL with opposite sign
  let copy = copy-literal-node(literal);
  copy.literal-negated-p := ~ copy.literal-negated-p;
  copy;
end method literal-flip-negation;

define method nliteral-flip-negation (literal)
  // Destructively modify LITERAL to have opposite sign
  literal.literal-negated-p := ~ literal.literal-negated-p;
  literal;
end method nliteral-flip-negation;

// ----------------------------------------------------------------------------
define method query-to-answer-literal (query)
  make-literal-node(relation: #"answer_", terms: find-vars(query));
end method query-to-answer-literal;

// ----------------------------------------------------------------------------
define method nliteral-rename-all-variables (literal)
  let bl = literal-rename-binding-list(literal);
  nliteral-plug(literal, bl);
end method nliteral-rename-all-variables;

define method literal-rename-binding-list (literal)
  map(method (x) pair(x, make-new-variable(x)); end method,
      cl-remove-duplicates(find-vars(literal.literal-terms)));
end method literal-rename-binding-list;

// ----------------------------------------------------------------------------
define method literal-vars-in (literal)
  // Returns list (set) of variables in terms of literal
  cl-remove-duplicates(find-vars(literal.literal-terms));
end method literal-vars-in;

// ----------------------------------------------------------------------------
define method function-depth (literal)
  // The maximum depth of function application to any term in LITERAL
  if (literal.literal-terms)
    tree-depth(literal.literal-terms) - 1;
  else
    0;
  end if;
end method function-depth;

// ----------------------------------------------------------------------------
// 
// 	Literal equality tests
// ----------------------------------------------------------------------------
define method literal-possible-negated-pair-p (lit1, lit2)
  // Negated pair, but knows about EVAL
  ~ (lit1.literal-negated-p == lit2.literal-negated-p)
   & lit1.literal-relation == lit2.literal-relation
   & dtp-unifyp(map(eval-to-var, lit1.literal-terms),
                map(eval-to-var, lit2.literal-terms));
end method literal-possible-negated-pair-p;

define method eval-to-var (term)
  if (instance?(term, <pair>) & first(term) == #"eval")
    make-new-variable(#"?eval");
  else
    term;
  end if;
end method eval-to-var;

// ----------------------------------------------------------------------------
define method literal-negated-pair-p (lit1, lit2, #key test = dtp-unifyp)
  // Returns unifying binding list, if negated pair, else nil
  lit1 & lit2 & ~ (lit1.literal-negated-p == lit2.literal-negated-p)
   & lit1.literal-relation == lit2.literal-relation
   & test(lit1.literal-terms, lit2.literal-terms);
end method literal-negated-pair-p;

// ----------------------------------------------------------------------------
define method literal-mgu (lit1, lit2, #key ignore-sign = #f)
  // Returns most general unifier of terms of both literals, if exists, else nil
  if (ignore-sign | lit1.literal-negated-p == lit2.literal-negated-p
       & lit1.literal-relation == lit2.literal-relation)
    dtp-unifyp(lit1.literal-terms, lit2.literal-terms);
  end if;
end method literal-mgu;

// ----------------------------------------------------------------------------
define method literal-instance (general-literal, instance-literal,
                                #key old-binding-list = #f)
  // True iff INSTANCE-LITERAL is an instance of GENERAL-LITERAL
  general-literal.literal-negated-p == instance-literal.literal-negated-p
   & general-literal.literal-relation == instance-literal.literal-relation
   & dtp-instp(general-literal.literal-terms, instance-literal.literal-terms,
               old-binding-list);
end method literal-instance;

// ----------------------------------------------------------------------------
define method literal-instance? (instance, general)
  // True IFF INSTANCE is more specific (or equal) to GENERAL
  literal-instance(general, instance);
end method literal-instance?;

// ----------------------------------------------------------------------------
define method literal-same-or-generalized-p (instance-literal,
                                             general-literal)
  // True iff I-LITERAL is same as or an instance of G-LITERAL
  instance-literal.literal-negated-p == general-literal.literal-negated-p
   & instance-literal.literal-relation == general-literal.literal-relation
   & dtp-instp(general-literal.literal-terms, instance-literal.literal-terms);
end method literal-same-or-generalized-p;

// ----------------------------------------------------------------------------
define method literal-equal-p (lit1, lit2)
  lit1.literal-negated-p == lit2.literal-negated-p
   & lit1.literal-relation == lit2.literal-relation
   & lit1.literal-terms = lit2.literal-terms;
end method literal-equal-p;

// ----------------------------------------------------------------------------
define method literal-samep (lit1, lit2)
  lit1.literal-negated-p == lit2.literal-negated-p
   & lit1.literal-relation == lit2.literal-relation
   & samep(lit1.literal-terms, lit2.literal-terms);
end method literal-samep;

// ----------------------------------------------------------------------------
"eof";

