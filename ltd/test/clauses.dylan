//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Clauses.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// A clause: List of literals in DNF
define class <clause-node> (<object>)
  slot clause-literals, init-keyword: #"clause-literals";
  slot clause-label = #f, init-keyword: #"clause-label";
end class <clause-node>;

// ----------------------------------------------------------------------------
define method clause-node-print-function (structure, stream, depth)
  format(stream, "<");
  print-clause-node(structure, s: stream);
  format(stream, ">");
end method clause-node-print-function;

define method print-clause-node (node, #key s = #t, as-rule = #f)
  if (*display-logic-as-lists*)
    print-clause-node-as-list(node, s: s);
  else
    if (refutation-clause-p(node))
      format(s, "[True]");
    elseif (as-rule)
      print-clause-node-as-rule(node, s: s);
    else
      print-clause-node-as-conjuncts(node, s: s);
    end if;
    if (node.clause-label)
      format(s, " with label %S", label-value(node.clause-label));
    end if;
  end if;
end method print-clause-node;

define method print-clause-node-as-list (node, #key s = #t)
  let lits = node.clause-literals;
  if (lits)
    print-literal-node(head(lits), s: s);
    for (lit in tail(lits))
      format-out(" ");
      print-literal-node(lit, s: s);
    end for;
  else
    format(s, "Nil");
  end if;
end method print-clause-node-as-list;

define method print-clause-node-as-rule (node, #key s = #t)
  let reverse = #f;
  let tail = copy-sequence(node.clause-literals);
  let head = #f;
  //  Partition clause into rule head and tail
  for (lit = first(tail) then first(tail),
       while lit & ~ literal-negated-p(lit))
    push!(pop!(tail), head);
  finally
    head := reverse!(head);
    #f;
  end for;
  //  Perhaps a "forward chaining" rule?
  if (~ head)
    tail := reverse!(tail);
    for (lit = first(tail) then first(tail),
         while lit & ~ literal-negated-p(lit))
      push!(pop!(tail), head);
    end for;
    tail := reverse!(tail);
    if (head) reverse := #t; end if;
    let g153318 = head;
    let g153317 = tail;
    head := g153317;
    tail := g153318;
    #f;
  end if;
  //  Print the head
  for (lit = pop!(head) then pop!(head), while lit)
    print-literal-node(lit, s: s, flip-negation: reverse);
    if (head)
      if (reverse) format(s, " and "); else format(s, " or "); end if;
    end if;
  end for;
  //  Print the tail
  if (tail)
    if (reverse) format(s, " => "); else format(s, " <= "); end if;
  end if;
  for (lit = pop!(tail) then pop!(tail), while lit)
    print-literal-node(lit, s: s, flip-negation: ~ reverse);
    if (tail)
      if (reverse) format(s, " or "); else format(s, " and "); end if;
    end if;
  end for;
end method print-clause-node-as-rule;

define method print-clause-node-as-conjuncts (node, #key s = #t)
  let lits = node.clause-literals;
  print-literal-node(head(lits), s: s);
  for (lit in tail(lits))
    format(s, " and ");
    print-literal-node(lit, s: s);
  end for;
end method print-clause-node-as-conjuncts;

// ----------------------------------------------------------------------------
define method refutation-clause-p (clause)
  empty?(clause.clause-literals);
end method refutation-clause-p;

define method clause-goal (clause)
  first(clause.clause-literals);
end method clause-goal;

define method clause-remaining-literals (clause)
  tail(clause.clause-literals);
end method clause-remaining-literals;

// ----------------------------------------------------------------------------
define method clause-plug (clause, binding-list)
  // Return a new clause which is a copy of CLAUSE with BINDING-LIST applied
  make-clause-node(literals: cl-remove-duplicates(map(method (old-lit)
                                                      literal-plug(old-lit,
                                                                   binding-list);
                                                      end method,
                                                      clause.clause-literals),
                                                  from-end: #t,
                                                  test: literal-equal-p),
                   label: clause.clause-label);
end method clause-plug;

define method nclause-plug (clause, binding-list)
  // Destructively modify CLAUSE by applying BINDING-LIST
  let list92543 = clause.clause-literals;
  begin
    do(method (old-lit) nliteral-plug(old-lit, binding-list); end method,
       list92543);
    list92543;
  end;
  clause.clause-literals
   := cl-remove-duplicates(clause.clause-literals, from-end: #t,
                           test: literal-equal-p);
  clause;
end method nclause-plug;

// ----------------------------------------------------------------------------
define method clause-remove (literal, clause)
  // Return a new clause which is a copy of CLAUSE with LITERAL removed
  make-clause-node(literals: remove(clause.clause-literals, literal),
                   label: clause.clause-label);
end method clause-remove;

// ----------------------------------------------------------------------------
define method clause-rename-all-variables (clause, #key except = #f)
  // Return a new clause and binding-list, copy of CLAUSE with vars renamed
  let bl = clause-rename-binding-list(clause, except: except);
  values(clause-plug(clause, bl), bl);
end method clause-rename-all-variables;

define method nclause-rename-all-variables (clause, #key except = #f)
  // Destructively modify CLAUSE by renaming all variables, return binding list
  let bl = clause-rename-binding-list(clause, except: except);
  nclause-plug(clause, bl);
  bl;
end method nclause-rename-all-variables;

define method clause-rename-binding-list (clause, #key except = #f)
  let orig-vars = #f;
  orig-vars := find-all-variables(clause.clause-literals);
  if (orig-vars)
    concatenate(begin
                  let _acc = make(<deque>);
                  for (ov in orig-vars)
                    if (~ cl-find(ov, except))
                      push-last(_acc, pair(ov, make-new-variable(ov)));
                    end if;
                  finally
                    _acc;
                  end for;
                end,
                #(#(#"t" . #"t")));
  else
    #(#(#"t" . #"t"));
  end if;
end method clause-rename-binding-list;

define method find-all-variables (literal-list)
  cl-remove-duplicates(apply(concatenate,
                             map(method (lit)
                                   find-vars(literal-terms(lit));
                                 end method,
                                 literal-list)),
                       from-end: #t);
end method find-all-variables;

// ----------------------------------------------------------------------------
define method clause-merge (lit1, c1, lit2, c2, binding-list)
  // Returns clause c1+c2-l1-l2
  let literal-list = #f;
  literal-list
   := map(method (old-lit) literal-plug(old-lit, binding-list); end method,
          concatenate(remove(c1.clause-literals, lit1),
                      remove(c2.clause-literals, lit2)));
  literal-list
   := cl-remove-duplicates(literal-list, from-end: #t, test: literal-equal-p);
  make-clause-node(literals: literal-list,
                   label: label-and(c1.clause-label, c2.clause-label));
end method clause-merge;

// ----------------------------------------------------------------------------
define method clause-is-instance-of-subset-p (c-inst, c-super, #key cdr = #f)
  // True if every literal in C-INST is an instance of some literal in C-SUPER
  let bl = #(#(#"t" . #"t"));
  let super-lits
      = if (cdr)
          tail(c-super.clause-literals);
        else
          c-super.clause-literals;
        end if;
  block (return-from-instance-loop)
    for (c-super-list = map(copy-literal-node,
                            super-lits) then map(method (x)
                                                 nliteral-plug(x, bl);
                                                 end method,
                                                 c-super-list),
         lit1 in c-inst.clause-literals)
      if (~ cl-find(lit1, c-super-list, test: literal-equal-p))
        block (return-from-superset-loop)
          for (lit2 in c-super-list,
               new-bl = literal-instance(lit2,
                                         lit1,
                                         bl) then literal-instance(lit2,
                                                                   lit1,
                                                                   bl))
            if (new-bl) bl := new-bl; return-from-superset-loop(#f); end if;
          finally
            return-from-instance-loop(#f);
            #f;
          end for;
        end block;
      end if;
    finally
      return-from-instance-loop(bl);
      #f;
    end for;
  end block;
end method clause-is-instance-of-subset-p;

// ----------------------------------------------------------------------------
define method clause-equal-p (clause1, clause2)
  let cl1 = clause1.clause-literals;
  let l1 = clause1.clause-label;
  let cl2 = clause2.clause-literals;
  let l2 = clause2.clause-label;
  empty?(l1) | empty?(l2)
   | (label-value(l1) = label-value(l2)
       & label-structure(l1) == label-structure(l2))
   & size(cl1) = size(cl2)
   & block (return)
       for (lit1 in cl1, lit2 in cl2)
         if (~ literal-equal-p(lit1, lit2)) return(#f); end if;
       finally
         return(#t);
         #f;
       end for;
     end block;
end method clause-equal-p;

// ----------------------------------------------------------------------------
define method clause-to-list (clause)
  let _acc = make(<deque>);
  for (literal in clause.clause-literals)
    push-last(_acc, literal-to-list(literal));
  finally
    _acc;
  end for;
end method clause-to-list;

// ----------------------------------------------------------------------------
define method clause-list-equal-p (clause, list, #key test = \=)
  block (return)
    for (literal in clause.clause-literals, list-literal in list)
      if (~ literal-list-equal-p(literal, list-literal, test: test))
        return(#f);
      end if;
    finally
      return(#t);
      #f;
    end for;
  end block;
end method clause-list-equal-p;

// ----------------------------------------------------------------------------
define method nclause-flip-negations (clause)
  block (return)
    for (literal in clause.clause-literals)
      literal-negated-p(literal) := ~ literal-negated-p(literal);
    finally
      return(clause);
      #f;
    end for;
  end block;
end method nclause-flip-negations;

// ----------------------------------------------------------------------------
"eof";

