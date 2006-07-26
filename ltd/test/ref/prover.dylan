//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Prover.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

define module dtp
  export prove, prove-next-answer, prove-all-remaining-answers;
end module dtp;

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method prove (query, #key all-answers = #f, return-form = #f,
                     suppress-disjunctive-answers = #f)
  // Returns (1) bound query(ies), (2) label(s), (3) <answer(s)>, (4) <proof>
  fluid-bind (*proof* = #f)
    *proof*
     := make-proof(query: query, return-form: return-form,
                   sda: suppress-disjunctive-answers);
    reset(*proof*);
    *last-proof* := *proof*;
    if (*timeout-maximum-seconds*)
      *timeout-count* := 0;
      *timeout-end*
       := // LTD: Function GET-INTERNAL-RUN-TIME not yet implemented.
          get-internal-run-time()
           + $internal-time-units-per-second * *timeout-maximum-seconds*;
    end if;
    if (all-answers)
      prove-all-remaining-answers(*proof*);
    else
      prove-next-answer(*proof*);
    end if;
  end fluid-bind;
end method prove;

// ----------------------------------------------------------------------------
define method prove-next-answer (#key *proof* = *proof*)
  // Returns (1) bound query, (2) label, (3) <answer> or :NOT-AN-ANSWER, and (4) <proof>
  explode-answer(prove-next-answer-with-iteration(*proof*));
end method prove-next-answer;

// ----------------------------------------------------------------------------
define method prove-all-remaining-answers (#key *proof* = *proof*)
  // Returns (1) queries, (2) labels, (3) residues, (4) <answer>s, (5) <proof>
  let answers = #f;
  let bounds = #f;
  let label-values = #f;
  let residues = #f;
  answers := prove-all-remaining-answers-internal(*proof*);
  bounds := map(apply-answer, answers);
  label-values := map(extract-label, answers);
  residues := map(extract-residue, answers);
  values(bounds, label-values, residues, answers, *proof*);
end method prove-all-remaining-answers;

// ----------------------------------------------------------------------------
define method propagate (answer, proof :: <proof>)
  if (cl-find(answer, proof-answers(proof), test: answer-equal-p)
       | cl-find(answer, proof-new-answers(proof), test: answer-equal-p))
    #f;
  else
    add-to-end(answer, proof-new-answers(proof));
  end if;
end method propagate;

// ----------------------------------------------------------------------------
define method apply-answer (answer, #key form = proof-return-form(*proof*))
  // Turn an answer structure into a user-readable form
  if (~ form) form := proof-query(*proof*); end if;
  if (answer-ae-binding-lists(answer))
    pair(#"or",
         pair(plug(form, answer-binding-list(answer)),
              map(method (bl) plug(form, bl); end method,
                  answer-ae-binding-lists(answer))));
  else
    plug(form, answer-binding-list(answer));
  end if;
end method apply-answer;

// ----------------------------------------------------------------------------
define method active-agenda (#key proof = *proof*)
  cl-find-if(active-p, proof-subgoal-agenda(proof));
end method active-agenda;

// ----------------------------------------------------------------------------
define method agenda-add (subgoal)
  // Add SUBGOAL to the proof agenda
  if (cl-find(subgoal, proof-subgoal-agenda(*proof*)))
    #f;
  else
    push!(subgoal, proof-subgoal-agenda(*proof*));
  end if;
end method agenda-add;

// ----------------------------------------------------------------------------
define method agenda-remove (subgoal)
  // Remove SUBGOAL from the proof agenda
  if (subgoal == first(proof-subgoal-agenda(*proof*)))
    pop!(proof-subgoal-agenda(*proof*));
  else
    proof-subgoal-agenda(*proof*)
     := remove(proof-subgoal-agenda(*proof*), subgoal);
  end if;
end method agenda-remove;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define variable *last-sc* = #f;

define variable *last-fc* = #f;

define method prove-next-answer-with-iteration (#key *proof* = *proof*)
  // Iterative deepening on subgoal and function depth
  *last-sc* := -1;
  *last-fc* := -1;
  let next-answer = #f;
  let s-depth
      = *use-subgoal-cutoffs*
         & (proof-subgoal-depth-cutoff(*proof*)
             | proof-subgoal-maximum-depth(*proof*)
             | *initial-subgoal-depth*
             | *subgoal-maximum-depth*
             | 0)
         | 0;
  let f-depth
      = *use-function-cutoffs*
         & (proof-function-depth-cutoff(*proof*)
             | proof-function-maximum-depth(*proof*)
             | *initial-function-depth*
             | *function-maximum-depth*
             | 0)
         | 0;
  block (return)
    for (while %t)
      //  Enforce maximums
      if (proof-subgoal-maximum-depth(*proof*)
           & s-depth > proof-subgoal-maximum-depth(*proof*))
        s-depth := proof-subgoal-maximum-depth(*proof*);
      end if;
      if (proof-function-maximum-depth(*proof*)
           & f-depth > proof-function-maximum-depth(*proof*))
        f-depth := proof-function-maximum-depth(*proof*);
      end if;
      //  Caching
      if (*caching* == #"answers") flush-answer-failure-cache(); end if;
      //  Do a bounded search
      proof-subgoal-depth-cutoff(*proof*) := s-depth;
      proof-function-depth-cutoff(*proof*) := f-depth;
      next-answer := prove-next-answer-internal();
      if (~ (next-answer == #"not-an-answer")) return(next-answer); end if;
      //  Loop exit conditions
      if (proof-subgoal-cutoff-occurred(*proof*))
        if (proof-function-cutoff-occurred(*proof*))
          if (~ (proof-subgoal-depth-skip(*proof*)
                  | proof-function-depth-skip(*proof*)))
            return(#"not-an-answer");
          end if;
        else
          if (~ proof-subgoal-depth-skip(*proof*))
            return(#"not-an-answer");
          end if;
        end if;
      elseif (proof-function-cutoff-occurred(*proof*))
        if (~ proof-function-depth-skip(*proof*))
          return(#"not-an-answer");
        end if;
      else
        return(#"not-an-answer");
      end if;
      //  Iterate to next depth
      if (*use-subgoal-cutoffs* & proof-subgoal-depth-skip(*proof*))
        s-depth := s-depth + proof-subgoal-depth-skip(*proof*);
      end if;
      if (*use-function-cutoffs* & proof-function-depth-skip(*proof*))
        f-depth := f-depth + proof-function-depth-skip(*proof*);
      end if;
      if (within-maxbounds-p(s-depth, f-depth))
        reset(*proof*);
      else
        return(#"not-an-answer");
      end if;
    end for;
  end block;
end method prove-next-answer-with-iteration;

// ----------------------------------------------------------------------------
define method within-maxbounds-p (subgoal-cutoff, function-cutoff)
  block (return-from-within-maxbounds-p)
    if (*last-sc* = subgoal-cutoff & *last-fc* = function-cutoff)
      return-from-within-maxbounds-p(#f);
    else
      *last-sc* := subgoal-cutoff;
      *last-fc* := function-cutoff;
    end if;
    proof-subgoal-maximum-depth(*proof*)
     & subgoal-cutoff <= proof-subgoal-maximum-depth(*proof*)
     | (proof-function-maximum-depth(*proof*)
         & function-cutoff <= proof-function-maximum-depth(*proof*))
     | (empty?(proof-subgoal-maximum-depth(*proof*))
         & empty?(proof-function-maximum-depth(*proof*)));
  end block;
end method within-maxbounds-p;

// ----------------------------------------------------------------------------
define method reset (proof :: <proof>)
  proof-query-conjunctions(proof)
   := map(list-to-conjunction, dnf(proof-query(proof)));
  if (*use-negated-goal*)
    fluid-bind (*goal-node-id-count* = 0)
      proof-goal-nodes(proof)
       := map(make-goal-node, proof-query-conjunctions(proof));
    end fluid-bind;
  end if;
  proof-blocked-conjunctions(proof) := #f;
  proof-subgoal-index(proof) := make(<table>, test: \==);
  proof-subgoal-cutoff-occurred(proof) := #f;
  proof-function-cutoff-occurred(proof) := #f;
end method reset;

// ----------------------------------------------------------------------------
define method prove-next-answer-internal ()
  // Return <answer> or :NOT-AN-ANSWER
  block (return)
    for (next-answer = pop!(proof-new-answers(*proof*)) then pop!(proof-new-answers(*proof*)),
         next-subgoal = agenda-best() then agenda-best(),
         next-conjunction = first(proof-query-conjunctions(*proof*)) then first(proof-query-conjunctions(*proof*)),
         while next-answer | next-subgoal | next-conjunction
                | (*use-unblocking* & forkable-conjunctions?()))
      if (*timeout-maximum-seconds*)
        inc!(*timeout-count*);
        if (*timeout-count* > *timeout-resolution*)
          *timeout-count* := 0;
          let rt
              = // LTD: Function GET-INTERNAL-RUN-TIME not yet implemented.
                get-internal-run-time();
          if (rt > *timeout-end*) return(#"not-an-answer"); end if;
        end if;
      end if;
      if (next-answer)
        begin
          let valid = process-answer(next-answer);
          if (valid) return(valid); end if;
        end;
      elseif (next-subgoal)
        process-subgoal(next-subgoal);
      elseif (*use-unblocking* & forkable-conjunctions?())
        unblock-agenda();
      else
        process-conjunction(next-conjunction);
      end if;
    finally
      return(#"not-an-answer");
      #f;
    end for;
  end block;
end method prove-next-answer-internal;

// ----------------------------------------------------------------------------
define method process-answer (answer)
  if (~ (proof-sda(*proof*) & disjunctive-p(answer)))
    add-to-end(answer, proof-answers(*proof*));
    answer;
  end if;
end method process-answer;

// ----------------------------------------------------------------------------
define method process-subgoal (subgoal)
  if (empty?(subgoal.conjuncts-to-propagate-to))
    agenda-remove(subgoal);
  elseif (exhausted-p(subgoal, ignore-blocked: #t))
    if (exhausted-p(subgoal)) terminate(subgoal); end if;
    agenda-remove(subgoal);
  else
    expand(subgoal);
  end if;
end method process-subgoal;

// ----------------------------------------------------------------------------
define method process-conjunction (conjunction)
  if (exhausted-p(conjunction))
    pop!(proof-query-conjunctions(*proof*));
  else
    expand(conjunction);
  end if;
end method process-conjunction;

// ----------------------------------------------------------------------------
define method prove-all-remaining-answers-internal (#key *proof* = *proof*)
  // Return list of all remaining <answers>
  let answers = make(<deque>);
  block (return)
    for (next-answer = prove-next-answer-with-iteration(*proof*) then prove-next-answer-with-iteration(*proof*),
         until next-answer == #"not-an-answer")
      push-last(answers, next-answer);
    finally
      return(answers);
      answers;
    end for;
  end block;
end method prove-all-remaining-answers-internal;

// ----------------------------------------------------------------------------
define method explode-answer (answer)
  // Returns (1) bound query, (2) label, (3) residue, (4) ANSWER, (5) *PROOF*
  if (answer-p(answer))
    values(apply-answer(answer), extract-label(answer),
           extract-residue(answer), answer, *proof*);
  else
    values(#f, #f, #f, not-an-answer: *proof*);
  end if;
end method explode-answer;

// ----------------------------------------------------------------------------
define method extract-label (answer)
  let label = answer-label(answer);
  if (label) label.label-value; end if;
end method extract-label;

// ----------------------------------------------------------------------------
define method extract-residue (answer)
  map(literal-to-list, answer-residue(answer));
end method extract-residue;

// ----------------------------------------------------------------------------
define method make-goal-node (conjunction)
  // Return clause corresponding to negated conjunction
  let literals = make(<deque>);
  block (return)
    for (conjunct in conjunction.list,
         literal = conjunct.literal then conjunct.literal,
         new-literal = copy-literal-node(literal) then copy-literal-node(literal))
      new-literal.literal-negated-p := ~ new-literal.literal-negated-p;
      push-last(literals, new-literal);
    finally
      return(make-kb-node(id: make-new-id("GOAL", inc!(*goal-node-id-count*)),
                          clause: make-clause-node(literals: literals)));
      literals;
    end for;
  end block;
end method make-goal-node;

// ----------------------------------------------------------------------------
"eof";

