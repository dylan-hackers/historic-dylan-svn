//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Caching.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
//  Postponement and Recursion caching
define method find-subgoal (conjunct)
  // Return (1) an old or new subgoal SAMEP as LITERAL, (2) binding list or nil
  let bl = #f;
  let literal = literal-in-context(conjunct);
  let sg = #f;
  sg := find-stored-subgoal(literal, subgoal-parent-of(conjunct));
  if (sg)
    inc!(proof-cache-count(*proof*));
    bl
     := dtp-samep-binding-list(literal-terms(sg.literal),
                               literal-terms(literal));
    possibly-decrease-subgoal-depth(sg, conjunct);
  else
    sg := make-new-subgoal(conjunct, literal);
    memo-subgoal(sg);
  end if;
  values(sg, bl);
end method find-subgoal;

define method possibly-decrease-subgoal-depth (subgoal, new-parent-conjunct)
  // Searching space as a graph, so when new cache link perhaps higher subgoal
  block (return-from-possibly-decrease-subgoal-depth)
    //  Not useful at the moment, so abort
    return-from-possibly-decrease-subgoal-depth(#f);
    let parent-sg = subgoal-parent-of(new-parent-conjunct);
    let alternate-height = #f;
    if (parent-sg)
      alternate-height := parent-sg.depth + 1;
    else
      alternate-height := 0;
    end if;
    if (alternate-height < subgoal.depth)
      subgoal.depth := alternate-height;
    end if;
  end block;
end method possibly-decrease-subgoal-depth;

// ----------------------------------------------------------------------------
//  Subgoal caching
define method remember-completed-subgoal (subgoal)
  let c-sg = subgoal-in-cache-p(subgoal);
  if (c-sg) #f; else push!(subgoal, proof-subgoal-cache(*proof*)); end if;
end method remember-completed-subgoal;

define method subgoal-in-cache-p (subgoal)
  // Returns matching subgoal or NIL
  cl-find(subgoal.literal, proof-subgoal-cache(*proof*),
          key: method (sg) sg.literal; end method,
          test: literal-same-or-generalized-p);
end method subgoal-in-cache-p;

define method solutions-to-subgoal (subgoal)
  // Returns (1) Matching subgoal or NIL, (2) List of Answers
  let c-sg = #f;
  c-sg
   := cl-find(subgoal.literal, proof-subgoal-cache(*proof*),
              key: method (sg) sg.literal; end method,
              test: literal-same-or-generalized-p);
  if (c-sg)
    values(c-sg, create-relevant-answers(subgoal, c-sg));
  else
    values(#f, #f);
  end if;
end method solutions-to-subgoal;

// ----------------------------------------------------------------------------
//  Answer caching (success and failure)
define method remember-success (literal)
  if (cl-find(literal, proof-success-cache(*proof*), test: literal-samep))
    #f;
  else
    push!(literal, proof-success-cache(*proof*));
  end if;
end method remember-success;

define method remember-failure (literal)
  if (cl-find(literal, proof-failure-cache(*proof*), test: literal-samep))
    #f;
  else
    push!(literal, proof-failure-cache(*proof*));
  end if;
end method remember-failure;

define method lookup-literal (literal)
  // Returns (1) Cached literal or NIL, (2) :success, :failure, or NIL
  let c-lit = #f;
  if (c-lit
       := cl-find(literal, proof-success-cache(*proof*),
                  test: literal-same-or-generalized-p))
    values(c-lit, #"success");
  elseif (c-lit
           := cl-find(literal, proof-failure-cache(*proof*),
                      test: literal-same-or-generalized-p))
    values(c-lit, #"failure");
  else
    values(#f, #f);
  end if;
end method lookup-literal;

define method flush-answer-failure-cache ()
  proof-failure-cache(*proof*) := #f;
end method flush-answer-failure-cache;

// ----------------------------------------------------------------------------
define method make-new-subgoal (conjunct, #key new-literal = #f)
  if (~ new-literal) new-literal := literal-in-context(conjunct); end if;
  let subgoal = #f;
  begin
    subgoal := make(<dtp-subgoal>, literal: new-literal);
    begin
      let g15958 = conjunct.parent-conjunction;
      g15958;
      let g15959 = subgoal;
      let g15960 = #"parent-subgoal";
      let g15961 = g15958.parent-subgoal;
      .inv-slot-value(g15959, g15960, g15961);
      let g15962 = subgoal;
      let g15963 = #"parent-conjunct";
      let g15964 = conjunct;
      .inv-slot-value(g15962, g15963, g15964);
      if (g15958.parent-subgoal)
        let g15965 = subgoal;
        let g15966 = #"depth";
        let g15967 = g15958.parent-subgoal.depth + 1;
        .inv-slot-value(g15965, g15966, g15967);
      else
        let g15968 = subgoal;
        let g15969 = #"depth";
        let g15970 = 0;
        .inv-slot-value(g15968, g15969, g15970);
      end if;
    end;
    subgoal;
  end;
end method make-new-subgoal;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define method literal-in-context (conjunct)
  let literal = #f;
  literal := literal-plug(conjunct.literal, conjunct.binding-list);
  literal := nsimplify-terms(literal);
  //  Hook for term rewriting
  literal;
end method literal-in-context;

// ----------------------------------------------------------------------------
define variable *current-recursion-ancestors* = #f;

define method find-stored-subgoal (literal, parent-subgoal)
  block (return-from-find-stored-subgoal)
    if (~ cl-find(*caching*, #(#"subgoals", #"recursion", #"postponement")))
      return-from-find-stored-subgoal(#f);
    end if;
    let sg-list = #f;
    sg-list := proof-subgoal-index(*proof*)[literal-relation(literal)];
    if (doing-recursion-caching())
      fluid-bind (*current-recursion-ancestors* = #f)
        if (parent-subgoal)
          *current-recursion-ancestors*
           := subgoal-ancestors-of(parent-subgoal, include-me: #t);
        end if;
        cl-find(literal, sg-list, test: recursion-samep);
      end fluid-bind;
    else
      cl-find(literal, sg-list,
              test: select (*caching*)
                      #"postponement"
                         => postponement-samep;
                      #"subgoals"
                         => subgoals-samep;
                      otherwise
                         => #f;
                    end select);
    end if;
  end block;
end method find-stored-subgoal;

// ----------------------------------------------------------------------------
define method postponement-samep (literal, match-subgoal)
  literal-samep(literal, match-subgoal.literal);
end method postponement-samep;

define method recursion-samep (literal, match-subgoal)
  literal-samep(literal, match-subgoal.literal)
   & cl-find(match-subgoal, *current-recursion-ancestors*);
end method recursion-samep;

define method subgoals-samep (literal, match-subgoal)
  literal-samep(literal, match-subgoal.literal) & exhausted-p(match-subgoal);
end method subgoals-samep;

// ----------------------------------------------------------------------------
define method doing-recursion-caching ()
  *caching* == #"recursion"
   | (*caching* == #"postponement" & *cache-size*
       & proof-cache-count(*proof*) > *cache-size*);
end method doing-recursion-caching;

// ----------------------------------------------------------------------------
define method memo-subgoal (subgoal)
  block (return-from-memo-subgoal)
    if (~ cl-find(*caching*, #(#"subgoals", #"recursion", #"postponement")))
      return-from-memo-subgoal(#f);
    end if;
    let relation = literal-relation(subgoal.literal);
    let table = proof-subgoal-index(*proof*);
    if (table[relation])
      push!(subgoal, table[relation]);
    else
      table[relation] := list(subgoal);
    end if;
  end block;
end method memo-subgoal;

// ----------------------------------------------------------------------------
define method create-relevant-answers (inst-subgoal, cache-subgoal)
  let i-terms = literal-terms(inst-subgoal.literal);
  let c-terms = literal-terms(cache-subgoal.literal);
  let _acc = make(<deque>);
  for (c-answer in choose(complement(answer-ae-binding-lists),
                          cache-subgoal.answers),
       c-sexp = plug(c-terms,
                     answer-binding-list(c-answer)) then plug(c-terms,
                                                              answer-binding-list(c-answer)),
       new-bl = matching-bl(i-terms, c-sexp) then matching-bl(i-terms,
                                                              c-sexp))
    if (new-bl)
      push-last(_acc,
                begin
                  let new-answer = copy-answer(c-answer);
                  answer-binding-list(new-answer) := new-bl;
                  new-answer;
                end);
    end if;
  finally
    _acc;
  end for;
end method create-relevant-answers;

define method matching-bl (new-sexp1, cache-sexp2)
  let cache-vars = cl-remove-duplicates(find-vars(cache-sexp2));
  let new-bl = #f;
  new-bl := dtp-unifyp(new-sexp1, cache-sexp2);
  new-bl
   := choose(complement(method (binding)
                          cl-find(head(binding), cache-vars)
                           | cl-find(tail(binding), cache-vars);
                        end method),
             new-bl);
  new-bl;
end method matching-bl;

// ----------------------------------------------------------------------------
"eof";

