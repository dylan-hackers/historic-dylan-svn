//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Subgoals.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method expand (subgoal :: <dtp-subgoal>)
  if (~ possibly-find-in-cache(subgoal)) expand-subgoal(subgoal); end if;
end method expand;

// ----------------------------------------------------------------------------
define method exhausted-p (subgoal :: <dtp-subgoal>, #key ignore-blocked = #f)
  // True iff no further answers will be coming from this SUBGOAL
  empty?(subgoal.inferences)
   & (ignore-blocked | empty?(subgoal.blocked-conjunctions));
end method exhausted-p;

define method virgin-p (subgoal :: <dtp-subgoal>)
  // True iff SUBGOAL has never been worked on
  subgoal.remaining-ancestor-subgoals == #"uninitialized"
   & subgoal.assumables == #"uninitialized"
   & subgoal.inferences == #"uninitialized";
end method virgin-p;

define method active-p (subgoal :: <dtp-subgoal>)
  // True iff SUBGOAL has more constructive work to do
  subgoal.inferences;
end method active-p;

define method blocked-p (subgoal :: <dtp-subgoal>)
  ~ subgoal.inferences & subgoal.blocked-conjunctions;
end method blocked-p;

// ----------------------------------------------------------------------------
define method propagate (answer, subgoal :: <dtp-subgoal>)
  begin
    answer := copy-answer(answer);
    answer.answer-subgoal := subgoal;
    //  If a reduction reached the uppermost goal, then not context-dependent
    answer.answer-context := remove(answer.answer-context, subgoal);
    //  Remove bindings that aren't relevant (unless they might be later)
    if (~ answer.answer-context)
      answer := nsimplify-binding-list(answer, subgoal);
    end if;
    if (cl-find(answer, subgoal.answers, test: answer-instance?))
      #f;
    else
      add-to-end(answer, subgoal.answers);
      possibly-cache(answer, subgoal);
      for (conjunct in subgoal.conjuncts-to-propagate-to)
        propagate(answer, conjunct);
      end for;
    end if;
  end;
end method propagate;

// ----------------------------------------------------------------------------
define method terminate (subgoal :: <dtp-subgoal>, #key and-cache = #t)
  // Let waiting conjuncts know that there are no more answers
  if (and-cache)
    if (cl-find(*caching*, #(#"failure", #"answers")))
      if (~ subgoal.answers) remember-failure(subgoal.literal); end if;
    elseif (*caching* == #"subgoals")
      remember-completed-subgoal(subgoal);
    end if;
  end if;
  for (conjunct in subgoal.conjuncts-to-propagate-to)
    propagate(not-an-answer: conjunct);
  end for;
end method terminate;

// ----------------------------------------------------------------------------
define generic subgoal-parent-of (object) ;

define method subgoal-parent-of (object :: <proof>)
  #f;
end method subgoal-parent-of;

define method subgoal-parent-of (object :: <dtp-subgoal>)
  object.parent-subgoal;
end method subgoal-parent-of;

define method subgoal-parent-of (object :: <dtp-conjunction>)
  object.parent-subgoal;
end method subgoal-parent-of;

define method subgoal-parent-of (object :: <dtp-conjunct>)
  object.parent-conjunction.parent-subgoal;
end method subgoal-parent-of;

// ----------------------------------------------------------------------------
// 
// 
// 
// 
// 
// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
nil(#f, nil(),
    nil(nil(),
        nil((nil(#f, nil(#f)))(nil(#f, #f)), (nil(#f, nil(#f)))(nil(#f, #f)),
            nil(nil(nil(#f), #f,
                    nil(nil(#f, #f), nil(#f, nil(#f)), nil(#f, nil(#f))),
                    nil(#f,
                        nil((nil(nil(#f)))(),
                            nil(nil(#f, #f, #f), #f,
                                nil((nil(#f))(nil(#f),
                                              nil(nil(#f,
                                                      #f,
                                                      #f,
                                                      #f,
                                                      #f,
                                                      nil(#f, nil(#f))),
                                                  #f)),
                                    (nil(#f))(nil(#f), nil(nil(#f), nil(#f))),
                                    nil(nil(#f))))),
                        nil(nil(#f), nil(#f))))))));

// ----------------------------------------------------------------------------
define method expand-reduction (subgoal)
  block (return-from-expand-reduction)
    if (~ *use-reduction*) return-from-expand-reduction(#f); end if;
    begin
      if (subgoal.remaining-ancestor-subgoals == #"uninitialized")
        subgoal.remaining-ancestor-subgoals := subgoal-ancestors-of(subgoal);
      end if;
      block (return)
        for (ancestor = pop!(subgoal
                             .remaining-ancestor-subgoals) then pop!(subgoal
                                                                     .remaining-ancestor-subgoals),
             while ancestor,
             answer = reduction(subgoal, ancestor) then reduction(subgoal,
                                                                  ancestor))
          if (answer) return(answer); end if;
        end for;
      end block;
    end;
  end block;
end method expand-reduction;

// ----------------------------------------------------------------------------
define method expand-residue (subgoal)
  block (return-from-expand-residue)
    if (~ *use-residue*) return-from-expand-residue(#f); end if;
    begin
      if (subgoal.assumables == #"uninitialized")
        subgoal.assumables := *proof*.proof-assumables;
      end if;
      block (return)
        for (assumable = pop!(subgoal
                              .assumables) then pop!(subgoal.assumables),
             while assumable,
             answer = residue-answer(subgoal,
                                     assumable) then residue-answer(subgoal,
                                                                    assumable))
          if (answer) return(answer); end if;
        end for;
      end block;
    end;
  end block;
end method expand-residue;

// ----------------------------------------------------------------------------
define method compute-inference (subgoal)
  // Returns list of conjunctions
  block (return-from-compute-inference)
    if (~ *use-subgoal-inference*) return-from-compute-inference(#f); end if;
    let sg-literal = subgoal.literal;
    let node-list-1 = *proof*.proof-goal-nodes;
    let node-list-2
        = remove-pure-literal-nodes(active-theory-contents(*proof*
                                                           .proof-theory,
                                                           literal-relation(subgoal
                                                                            .literal)));
    let _acc = #();
    for (resolving-with-goal-p = node-list-1 then node-list-1,
         kb-node = if (node-list-1)
                     pop!(node-list-1);
                   else
                     pop!(node-list-2);
                   end if then if (node-list-1)
                                 pop!(node-list-1);
                               else
                                 pop!(node-list-2);
                               end if,
         while kb-node)
      _acc
       := concatenate(_acc,
                      begin
                        let c-bl
                            = begin
                                let (#rest _)
                                    = clause-rename-all-variables(kb-node
                                                                  .kb-node-clause);
                                _;
                              end;
                        let clause = first(c-bl);
                        let tbl = second(c-bl);
                        let _acc = make(<deque>);
                        for (literal in if (*use-contrapositives*)
                                        clause.clause-literals;
                                        else
                                        list(first(clause.clause-literals));
                                        end if,
                             mgu = literal-mgu(literal,
                                               sg-literal) then literal-mgu(literal,
                                                                            sg-literal))
                          if (mgu)
                            push-last(_acc,
                                      resolve(mgu,
                                              sg-literal,
                                              literal,
                                              clause,
                                              subgoal,
                                              kb-node.kb-node-id,
                                              if (resolving-with-goal-p)
                                              tbl;
                                              end if));
                          end if;
                        finally
                          _acc;
                        end for;
                      end);
    finally
      _acc;
    end for;
  end block;
end method compute-inference;

// ----------------------------------------------------------------------------
define method resolve (mgu, sg-lit, kb-lit, clause, parent, kb-parent,
                       ans-ext-bl)
  // Returns conjunction
  ans-ext-bl := remove(ans-ext-bl, #(#"t" . #"t"), test: \=);
  let sg-vars = find-vars(sg-lit.literal-terms);
  let bl = #f;
  let aebl = #f;
  let conjunction = #f;
  clause := copy-clause-node(clause);
  clause.clause-literals := remove(clause.clause-literals, kb-lit);
  clause := clause-plug(clause, mgu);
  nclause-flip-negations(clause);
  bl
   := remove(mgu,
             complement(method (binding)
                          cl-find(binding-variable(binding), sg-vars);
                        end method));
  aebl := simplify-ae-bl(plug(ans-ext-bl, mgu));
  if (~ different-binding-lists(bl, aebl)) aebl := #f; end if;
  conjunction
   := make(<dtp-conjunction>, parent: parent, binding-list: bl,
           label: clause.clause-label, ae-binding-list: aebl);
  conjunction.list
   := map(method (lit)
            make(<dtp-conjunct>, literal: lit, parent: conjunction);
          end method,
          clause.clause-literals);
  conjunction;
end method resolve;

// ----------------------------------------------------------------------------
define method simplify-ae-bl (binding-list)
  let _acc = make(<deque>);
  for (binding in binding-list)
    if (~ (binding-variable(binding) == binding-value(binding)))
      push-last(_acc, binding);
    end if;
  finally
    _acc;
  end for;
end method simplify-ae-bl;

// ----------------------------------------------------------------------------
define method subgoal-ancestors-of (subgoal :: <dtp-subgoal>,
                                    #key include-me = #f, #"#all-keys")
  // List of all direct parent subgoals above SUBGOAL in the proof graph
  if (subgoal) subgoal-ancestors-of-internal(subgoal, #f, include-me); end if;
end method subgoal-ancestors-of;

define method subgoal-ancestors-of (conjunct :: <dtp-conjunct>,
                                    #key #all-keys)
  let parent = subgoal-parent-of(conjunct);
  if (parent) subgoal-ancestors-of(parent, include-me: #t); end if;
end method subgoal-ancestors-of;

define method subgoal-ancestors-of-internal (subgoal,
                                             ancestors-so-far,
                                             include-me)
  if (cl-find(subgoal, ancestors-so-far))
    ancestors-so-far;
  else
    if (include-me) push!(subgoal, ancestors-so-far); end if;
    if (subgoal.parent-subgoal)
      subgoal-ancestors-of-internal(subgoal.parent-subgoal, ancestors-so-far,
                                    #t);
    else
      ancestors-so-far;
    end if;
  end if;
end method subgoal-ancestors-of-internal;

// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
nil(#f, nil(#f),
    nil((nil(nil(#f, #())))(nil(nil(#f, #())), #f), nil(#f, nil(#f, #f)),
        nil(#f, nil(#f, nil(#f), #f, nil(#f)))));

// ----------------------------------------------------------------------------
// 
// 	Pure Literal Elimination
define method remove-pure-literal-nodes (nodes)
  block (return-from-remove-pure-literal-nodes)
    if (~ *use-pure-literal-elimination*)
      return-from-remove-pure-literal-nodes(nodes);
    end if;
    let _acc = make(<deque>);
    for (node :: <kb-node> in nodes)
      if (~ pure-literal-node-p(node)) push-last(_acc, node); end if;
    finally
      _acc;
    end for;
  end block;
end method remove-pure-literal-nodes;

define method pure-literal-node-p (node)
  let pure-p = #f;
  pure-p
   := element(*proof*.proof-pure-literal-nodes, node.kb-node-id,
              default: #"unknown");
  if (pure-p == #"unknown")
    pure-p
     := block (return-from-check-pure)
          for (literal in node.kb-node-clause.clause-literals)
            if (~ can-find-matching-literal(literal, except: node))
              return-from-check-pure(#t);
            end if;
          finally
            return-from-check-pure(#f);
            #f;
          end for;
        end block;
    *proof*.proof-pure-literal-nodes[node.kb-node-id] := pure-p;
  end if;
  pure-p;
end method pure-literal-node-p;

define method can-find-matching-literal (literal, #key except = #f)
  block (return-from-can-find-matching-literal)
    //  Matching in query or (active) database...
    block (return)
      for (kb-node in concatenate(*proof*.proof-goal-nodes,
                                  remove(active-theory-contents(*proof*
                                                                .proof-theory,
                                                                literal
                                                                .literal-relation),
                                         except)))
        begin
          let test-clause
              = clause-rename-all-variables(kb-node.kb-node-clause);
          for (kb-literal in test-clause.clause-literals)
            if (literal-possible-negated-pair-p(literal, kb-literal))
              return-from-can-find-matching-literal(kb-node);
            end if;
          end for;
        end;
      finally
        return(#f);
        #f;
      end for;
    end block
     | //  ...or assumptions
    cl-find(literal, *proof*.proof-assumables, test: literal-negated-pair-p);
  end block;
end method can-find-matching-literal;

// ----------------------------------------------------------------------------
define method has-identical-ancestor (subgoal)
  // T iff subgoal has an identical parent
  let sg-literal = subgoal.literal;
  block (return)
    for (ancestor in subgoal-ancestors-of(subgoal),
         ans-literal = ancestor.literal then ancestor.literal)
      if (literal-equal-p(sg-literal, ans-literal)) return(#t); end if;
    finally
      return(#f);
      #f;
    end for;
  end block;
end method has-identical-ancestor;

// ----------------------------------------------------------------------------
define method possibly-cache (answer, subgoal)
  if (cl-find(*caching*, #(#"success", #"answers")))
    if (~ (answer.answer-label | answer.answer-residue
            | answer.answer-ae-binding-lists))
      remember-success(literal-plug(subgoal.literal,
                                    answer.answer-binding-list));
    end if;
  elseif (nil);
  end if;
end method possibly-cache;

// ----------------------------------------------------------------------------
define method exhaust (subgoal)
  begin
    subgoal.remaining-ancestor-subgoals := #f;
    subgoal.assumables := #f;
    subgoal.inferences := #f;
  end;
end method exhaust;

// ----------------------------------------------------------------------------
define method possibly-find-in-cache (subgoal)
  // Returns T if found (and uses cache), NIL if not found
  if (~ virgin-p(subgoal))
    #f;
  elseif (*caching* == #"iap" & has-identical-ancestor(subgoal))
    agenda-remove(subgoal);
    exhaust(subgoal);
    terminate(subgoal, #f);
    #t;
  elseif (cl-find(*caching*, #(#"success", #"failure", #"answers")))
    let (c-literal, where) = lookup-literal(subgoal.literal);
    select (where)
      #"success"
         => exhaust(subgoal); propagate(make-answer(), subgoal); #t;
      #"failure"
         => exhaust(subgoal); terminate(subgoal, #f); #t;
      otherwise
         => #f;
    end select;
  elseif (*caching* == #"subgoals")
    let (c-sg, answers) = solutions-to-subgoal(subgoal);
    if (c-sg)
      exhaust(subgoal);
      for (answer in answers) propagate(answer, subgoal); end for;
      #t;
    else
      #f;
    end if;
  else
    //  No caching, :recursion, or :postponement
    #f;
  end if;
end method possibly-find-in-cache;

// ----------------------------------------------------------------------------
"eof";

