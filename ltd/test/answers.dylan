//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Answers.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method answer-equal-p (ans1, ans2)
  label-equal-p(answer-label(ans1), answer-label(ans2))
   & residue-equal-p(answer-residue(ans1), answer-residue(ans2))
   & set-equal(pair(answer-binding-list(ans1), answer-ae-binding-lists(ans1)),
               pair(answer-binding-list(ans2), answer-ae-binding-lists(ans2)),
               same-binding-list)
   & set-equal(answer-context(ans1), answer-context(ans2));
end method answer-equal-p;

// ----------------------------------------------------------------------------
define method answer-instance? (instance, general)
  // True IFF INSTANCE is more specific (or equal) to GENERAL
  label-instance?(answer-label(instance), answer-label(general))
   & residue-instance?(answer-residue(instance), answer-residue(general))
   & subset?(answer-context(general), answer-context(instance))
   & subset?(pair(answer-binding-list(general),
                  answer-ae-binding-lists(general)),
             pair(answer-binding-list(instance),
                  answer-ae-binding-lists(instance)),
             test: binding-list-more-general?);
end method answer-instance?;

// ----------------------------------------------------------------------------
define method merge-answers (answer-list)
  // Return single answer that is the combination of all answers in the list
  block (return-from-merge-answers)
    let new-bl = #f;
    let new-label = #f;
    let new-residue = #f;
    let new-ae-binding-lists = #f;
    let new-reduction-subgoals = #f;
    let bls = map(answer-binding-list, answer-list);
    bls := remove(bls, #f);
    if (tail(bls))
      new-bl := merge-binding-lists(bls);
      if (new-bl == #"not-a-binding-list")
        return-from-merge-answers(#"not-an-answer");
      end if;
    else
      new-bl := first(bls);
    end if;
    let labels = map(answer-label, answer-list);
    labels := remove(labels, #f);
    if (tail(labels))
      new-label := reduce1(label-and, labels);
    else
      new-label := first(labels);
    end if;
    let residues = map(answer-residue, answer-list);
    residues := remove(residues, #f);
    if (tail(residues))
      new-residue := reduce1(residue-merge, residues);
      if (new-residue == #"not-a-residue")
        return-from-merge-answers(#"not-an-answer");
      end if;
    else
      new-residue := first(residues);
    end if;
    let ae-bls = map(answer-ae-binding-lists, answer-list);
    new-ae-binding-lists := reduce1(concatenate, ae-bls);
    let r-subgoals = map(answer-context, answer-list);
    new-reduction-subgoals
     := cl-remove-duplicates(reduce1(concatenate, r-subgoals));
    //  Construct new answer
    make-answer(binding-list: new-bl, context: new-reduction-subgoals,
                label: new-label, residue: new-residue,
                ae-binding-lists: new-ae-binding-lists);
  end block;
end method merge-answers;

// ----------------------------------------------------------------------------
define method nanswer-merge-binding-list (answer, binding-list)
  // Merge BINDING-LIST on to the binding list of ANSWER, or :NOT-AN-ANSWER
  if (answer-binding-list(answer))
    answer-binding-list(answer)
     := merge-binding-lists(list(answer-binding-list(answer), binding-list));
    if (answer-binding-list(answer) == #"not-a-binding-list")
      #"not-an-answer";
    else
      answer;
    end if;
  else
    answer-binding-list(answer) := binding-list;
    answer;
  end if;
end method nanswer-merge-binding-list;

// ----------------------------------------------------------------------------
define method nanswer-merge-label (answer, label)
  // Merge LABEL with answer label
  if (answer-label(answer))
    answer-label(answer) := label-and(answer-label(answer), label);
  else
    answer-label(answer) := label;
  end if;
  answer;
end method nanswer-merge-label;

// ----------------------------------------------------------------------------
define method nanswer-merge-residue (answer, residue)
  // Merge RESIDUE with answer residue
  if (answer-residue(answer))
    answer-residue(answer) := residue-merge(answer-residue(answer), residue);
    if (answer-residue(answer) == #"not-a-residue")
      #"not-an-answer";
    else
      answer;
    end if;
  else
    answer;
  end if;
end method nanswer-merge-residue;

// ----------------------------------------------------------------------------
define method answer-binds-var-p (answer, variable)
  cl-find(variable, answer-binding-list(answer), key: binding-variable);
end method answer-binds-var-p;

// ----------------------------------------------------------------------------
define method nsimplify-binding-list (answer, subgoal)
  // Remove any bindings for variables not in the subgoal literal
  let good-vars = literal-vars-in(subgoal.literal);
  let new-abl = #f;
  let new-ae-bls = #f;
  new-abl
   := remove(answer-binding-list(answer),
             complement(method (binding)
                          cl-find(binding-variable(binding), good-vars);
                        end method));
  new-ae-bls
   := remove(answer-ae-binding-lists(answer), new-abl,
             test: same-binding-list);
  new-ae-bls := cl-remove-duplicates(new-ae-bls, test: same-binding-list);
  answer-binding-list(answer) := new-abl;
  answer-ae-binding-lists(answer) := new-ae-bls;
  answer;
end method nsimplify-binding-list;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define method different-binding-lists (bl1, bl2)
  // True IFF the bindings are semantically distinct
  // LTD: Function SET-EXCLUSIVE-OR not yet implemented.
  set-exclusive-or(remove(bl1, #t,
                          test: method (x, y) x == head(y); end method),
                   remove(bl2, #t,
                          test: method (x, y) x == head(y); end method),
                   test: \=);
end method different-binding-lists;

// ----------------------------------------------------------------------------
define method same-binding-list (bl1, bl2)
  // True IFF the binding lists are semantically identical
  ~ different-binding-lists(bl1, bl2);
end method same-binding-list;

// ----------------------------------------------------------------------------
define method binding-list-more-general? (general, instance)
  // True IFF INSTANCE is more specific (or equal) to GENERAL
  subset?(general, instance, test: equal-binding?);
end method binding-list-more-general?;

define method equal-binding? (b1, b2)
  // True IFF B1 and B2 are identical bindings
  binding-variable(b1) == binding-variable(b2)
   & binding-value(b1) = binding-value(b2);
end method equal-binding?;

// ----------------------------------------------------------------------------
define method disjunctive-p (answer)
  // True IFF ANSWER is not disjuctive
  answer-ae-binding-lists(answer);
end method disjunctive-p;

// ----------------------------------------------------------------------------
"eof";

