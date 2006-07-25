//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Conjunct.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method expand (conjunct :: <dtp-conjunct>)
  begin
    if (conjunct.subgoal == #"uninitialized")
      if (cl-find(*caching*, #(#"recursion", #"postponement")))
        let (new-subgoal, tbl) = find-subgoal(conjunct);
        conjunct.subgoal := new-subgoal;
        conjunct.transform-binding-list := tbl;
      else
        conjunct.subgoal := make-new-subgoal(conjunct);
      end if;
      conjunct.nogoods := #f;
    end if;
    begin
      let g154775 = conjunct.subgoal;
      let answer = get-next-answer(conjunct.subgoal, conjunct);
      if (answer)
        inc!(conjunct.(conjunct.answer-count));
        propagate(answer, conjunct.parent-conjunction);
      elseif (exhausted-p(conjunct.subgoal))
        propagate(not-an-answer: conjunct.parent-conjunction);
      else
        begin
          let new-value-154777 = conjunct;
          let g154776
              = add!(new-value-154777, g154775.conjuncts-to-propagate-to);
          set-slot-value(g154775, #"conjuncts-to-propagate-to", g154776);
        end;
        if (~ cl-find(conjunct.subgoal, proof-subgoal-agenda(*proof*)))
          agenda-add(conjunct.subgoal);
        end if;
        propagate(blocked: conjunct.parent-conjunction);
      end if;
    end;
  end;
end method expand;

// ----------------------------------------------------------------------------
define method propagate (answer, conjunct :: <dtp-conjunct>)
  block (return-from-propagate)
    begin
      //  Optional error checking: should never occur
      if (~ (parent-conjunction.stack-pointer >= 0
              & parent-conjunction.list[parent-conjunction.stack-pointer]
                                         == conjunct))
        return-from-propagate(#f);
      end if;
      if (answer == #"not-an-answer")
        propagate(not-an-answer: conjunct.parent-conjunction);
      else
        let (failure, ancestors)
            = invalid-context(answer, conjunct,
                              conjunct.transform-binding-list);
        if (failure)
          #f;
        else
          if (conjunct.transform-binding-list)
            answer := copy-answer(answer);
            answer-binding-list(answer)
             := plug(answer-binding-list(answer),
                     conjunct.transform-binding-list);
            answer-context(answer) := ancestors;
          end if;
          inc!(conjunct.answer-count);
          propagate(answer, conjunct.parent-conjunction);
        end if;
      end if;
    end;
  end block;
end method propagate;

// ----------------------------------------------------------------------------
define method reset (conjunct :: <dtp-conjunct>)
  // Backtracking over CONJUNCT, so return to uninitialized state
  begin
    if (~ (conjunct.subgoal == #"uninitialized"))
      unattach(conjunct);
      conjunct.subgoal := #"uninitialized";
    end if;
    conjunct.answer-count := 0;
    conjunct.nogoods := #"uninitialized";
  end;
end method reset;

define method unattach (conjunct)
  // Remove CONJUNCT from master subgoal propagate list
  if (~ (conjunct.subgoal == #"uninitialized"))
    let g154970 = conjunct.subgoal;
    if (cl-find(conjunct, g154970.conjuncts-to-propagate-to))
      g154970.conjuncts-to-propagate-to
       := remove(g154970.conjuncts-to-propagate-to, conjunct);
      if (empty?(g154970.conjuncts-to-propagate-to))
        agenda-remove(conjunct.subgoal);
      end if;
    end if;
  end if;
end method unattach;

// ----------------------------------------------------------------------------
define method copy-conjunct (conj)
  // Should be defined by CLOS, but for some reason isn't
  let new = #f;
  new := make(<dtp-conjunct>);
  for (slot in #(#"literal", #"parent-conjunction", #"binding-list",
                 #"transform-binding-list", #"answer-count", #"subgoal",
                 #"nogoods"))
    new.slot := conj.slot;
  end for;
  new;
end method copy-conjunct;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define method get-next-answer (subgoal, conjunct)
  // Get the next answer from SUBGOAL valid in CONJUNCT's context
  block (return-from-get-next-answer)
    if (original-parent?(subgoal, conjunct))
      let answer = #f;
      answer := subgoal.answers[conjunct.answer-count];
      return-from-get-next-answer(answer);
    end if;
    let tbl = conjunct.transform-binding-list;
    block (return)
      for (answer = subgoal
                    .answers[conjunct
                             .answer-count] then subgoal
                                                 .answers[conjunct
                                                          .answer-count],
           while answer)
        let (failure, ancestors) = invalid-context(answer, conjunct, tbl);
        if (failure)
          inc!(conjunct.answer-count);
        else
          answer := copy-answer(answer);
          answer-binding-list(answer)
           := plug(answer-binding-list(answer), tbl);
          if (ancestors) answer-context(answer) := ancestors; end if;
          return(answer);
        end if;
      finally
        return(answer);
        #f;
      end for;
    end block;
  end block;
end method get-next-answer;

// ----------------------------------------------------------------------------
define method invalid-context (answer, conjunct, tbl)
  // Returns (1) residue of literals/T if invalid, (2) needed ancestors
  if (empty?(answer-context(answer)))
    values(#f, #f);
    //  Propagating up original line
    elseif (original-parent?(answer-subgoal(answer), conjunct))
    values(#f, #f);
    //  (Perhaps) don't copy reductions answers across cache link
    elseif (~ *cache-reductions*)
    values(#t, #f);
    //  Otherwise, copy answer if new context is sufficient
    else
    begin
      let literals
          = map(method (s) literal-plug(s.literal, tbl); end method,
                answer-context(answer));
      let used-ancestors = make(<deque>);
      block (return)
        for (while literals, ancestor in subgoal-ancestors-of(conjunct),
             match = cl-find(ancestor.literal, literals,
                             test: literal-equal-p) then cl-find(ancestor
                                                                 .literal,
                                                                 literals,
                                                                 test: literal-equal-p))
          if (match) push-last(used-ancestors, ancestor); end if;
          if (match) literals := remove(literals, match); end if;
        finally
          return(literals, used-ancestors);
          used-ancestors;
        end for;
      end block;
    end;
  end if;
end method invalid-context;

// ----------------------------------------------------------------------------
define method original-parent? (subgoal, conjunct)
  // True IFF CONJUNCT was the original parent of SUBGOAL
  if (subgoal) subgoal.parent-conjunct == conjunct; end if;
end method original-parent?;

// ----------------------------------------------------------------------------
// 
// 
// ----------------------------------------------------------------------------
"eof";

