//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Conjunctions.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method expand (conjunction :: <dtp-conjunction>)
  if (~ (conjunction.stack-pointer < 0))
    let conjunct = conjunction.list[conjunction.stack-pointer];
    if (conjunct)
      expand(conjunct);
    else
      let answer = #f;
      answer := merge-conjunction-answers(conjunction);
      if (~ (answer == #"not-an-answer"))
        note-solution(conjunction);
        if (conjunction.parent-subgoal)
          propagate(answer, conjunction.parent-subgoal);
        else
          propagate(answer, *proof*);
        end if;
      end if;
      pop!(conjunction.stack);
      dec!(conjunction.stack-pointer);
    end if;
  end if;
end method expand;

// ----------------------------------------------------------------------------
define method propagate (answer, conjunction :: <dtp-conjunction>)
  block (return-from-propagate)
    begin
      update-blocking(conjunction, answer);
      select (answer)
        #"blocked"
           => #f;
        #"not-an-answer"
           => backtrack(conjunction);
        otherwise
           => let this-conjunct = conjunction.list[conjunction.stack-pointer];
               let next-conjunct
                   = conjunction.list[inc!(conjunction.stack-pointer)];
               let mgu = #f;
               unattach(this-conjunct);
               push!(answer, conjunction.stack);
               if (next-conjunct)
                 mgu := merge-conjunction-binding-lists(conjunction);
                 if (mgu == #"not-a-binding-list")
                   pop!(conjunction.stack);
                   dec!(conjunction.stack-pointer);
                   return-from-propagate(#f);
                 else
                   next-conjunct.binding-list := mgu;
                 end if;
               end if;
               expand(conjunction);
      end select;
    end;
  end block;
end method propagate;

// ----------------------------------------------------------------------------
define method exhausted-p (conjunction :: <dtp-conjunction>, #key #all-keys)
  conjunction.stack-pointer < 0;
end method exhausted-p;

define method virgin-p (conjunction :: <dtp-conjunction>)
  // True IFF CONJUNCTION has never been worked on
  empty?(conjunction.list)
   | (conjunction.list & conjunction.stack-pointer = 0
       & first(conjunction.list).subgoal == #"uninitialized");
end method virgin-p;

define method active-conjunct (conjunction)
  if (conjunction.stack-pointer >= 0)
    conjunction.list[conjunction.stack-pointer];
  end if;
end method active-conjunct;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define method merge-conjunction-answers (conjunction)
  // Returns merge of answers (plus conj bl), or :NOT-AN-ANSWER
  block (return-from-merge-conjunction-answers)
    let answer = merge-answers(conjunction.stack);
    if (answer == #"not-an-answer")
      return-from-merge-conjunction-answers(#"not-an-answer");
    end if;
    if (conjunction.binding-list)
      answer := nanswer-merge-binding-list(answer, conjunction.binding-list);
      if (answer == #"not-an-answer")
        return-from-merge-conjunction-answers(#"not-an-answer");
      end if;
    end if;
    if (conjunction.label)
      answer := nanswer-merge-label(answer, conjunction.label);
    end if;
    if (conjunction.residue)
      answer := nanswer-merge-residue(answer, conjunction.residue);
      if (answer == #"not-an-answer")
        return-from-merge-conjunction-answers(#"not-an-answer");
      end if;
    end if;
    if (conjunction.ae-binding-list)
      push!(conjunction.ae-binding-list, answer-ae-binding-lists(answer));
    end if;
    answer;
  end block;
end method merge-conjunction-answers;

// ----------------------------------------------------------------------------
define method merge-conjunction-binding-lists (conjunction)
  let bls = map(answer-binding-list, conjunction.stack);
  bls := pair(conjunction.binding-list, bls);
  bls := remove(bls, #f);
  if (tail(bls)) merge-binding-lists(bls); else first(bls); end if;
end method merge-conjunction-binding-lists;

// ----------------------------------------------------------------------------
define method merge-conjunction-labels (conjunction)
  let labels = map(answer-label, conjunction.stack);
  labels := pair(conjunction.label, labels);
  labels := remove(labels, #f);
  if (tail(labels)) reduce1(label-and, labels); else first(labels); end if;
end method merge-conjunction-labels;

// ----------------------------------------------------------------------------
define method merge-conjunction-residues (conjunction)
  let residues = map(answer-residue, conjunction.stack);
  residues := pair(conjunction.residue, residues);
  residues := remove(residues, #f);
  if (tail(residues))
    let merge-res = reduce1(residue-merge, residues);
    if (merge-res == #"not-a-residue")
      #"not-a-residue";
    else
      merge-res;
    end if;
  else
    first(residues);
  end if;
end method merge-conjunction-residues;

// ----------------------------------------------------------------------------
define method list-to-conjunction (list)
  let conjuncts = make(<deque>);
  block (return)
    for (sublist in list)
      push-last(conjuncts,
                make(<dtp-conjunct>, literal: list-to-literal(sublist)));
    finally
      begin
        let conjunction = #f;
        conjunction := make(<dtp-conjunction>, list: conjuncts);
        for (conjunct in conjuncts)
          conjunct.parent-conjunction := conjunction;
        end for;
        return(conjunction);
      end;
      conjuncts;
    end for;
  end block;
end method list-to-conjunction;

// ----------------------------------------------------------------------------
define method debug-print (conjunction)
  // Concise summary of object
  for (remaining = conjunction.list then tail(remaining),
       until empty?(remaining),
       literal = first(remaining).literal then first(remaining).literal,
       in-middle = %f then %t)
    if (in-middle) format(*debug-io*, "^"); end if;
    print-literal-node(literal, s: *debug-io*);
  end for;
end method debug-print;

// ----------------------------------------------------------------------------
define method note-solution (conjunction)
  // Solution found, so must backtrack instead of backjump
  conjunction.backtrack-pointer := size(conjunction.list) - 1;
end method note-solution;

// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
"eof";

