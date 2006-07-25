//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Fork.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method update-blocking (conjunction, answer)
  if (answer == #"blocked")
    //  Block the conjunction
    if (parent-subgoal)
      parent-subgoal.inferences
       := remove(parent-subgoal.inferences, conjunction);
      add-to-end-if-new(conjunction, parent-subgoal.blocked-conjunctions);
    else
      proof-query-conjunctions(*proof*)
       := remove(proof-query-conjunctions(*proof*), conjunction);
      add-to-end-if-new(conjunction, proof-blocked-conjunctions(*proof*));
    end if;
  elseif (parent-subgoal)
    if (~ cl-find(conjunction, parent-subgoal.inferences))
      push!(conjunction, parent-subgoal.inferences);
      parent-subgoal.blocked-conjunctions
       := remove(parent-subgoal.blocked-conjunctions, conjunction);
    end if;
  else
    if (~ cl-find(conjunction, proof-query-conjunctions(*proof*)))
      push!(conjunction, proof-query-conjunctions(*proof*));
      proof-blocked-conjunctions(*proof*)
       := remove(proof-blocked-conjunctions(*proof*), conjunction);
    end if;
  end if;
end method update-blocking;

// ----------------------------------------------------------------------------
define method fork-conjunction (conj)
  let forked = #f;
  forked := copy-conjunction(conj);
  fork-specialize!(forked, conj);
  let parent = forked.parent-subgoal;
  if (parent)
    add-to-end(forked, parent.blocked-conjunctions);
  else
    add-to-end(forked, proof-blocked-conjunctions(*proof*));
  end if;
end method fork-conjunction;

// ----------------------------------------------------------------------------
define method unblock-agenda ()
  // All remaining subgoals blocked, so proof effort is in cycle
  let blocked-conj = #f;
  blocked-conj := best-blocked-conjunction();
  fork-conjunction(blocked-conj);
  blocked-conj.backtrack-pointer := blocked-conj.stack-pointer;
  propagate(not-an-answer: active-conjunct(blocked-conj));
end method unblock-agenda;

// ----------------------------------------------------------------------------
define method forkable-conjunctions? ()
  // True IFF the subgoal agenda or query has a forkable conjunction
  cl-find-if(forkable?, proof-blocked-conjunctions(*proof*))
   | cl-find-if(method (sg)
                  cl-find-if(forkable?, sg.blocked-conjunctions);
                end method,
                proof-subgoal-agenda(*proof*));
end method forkable-conjunctions?;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define method best-blocked-conjunction ()
  // Return a blocked conjunction whose unblocking might make progress
  let conjs = #f;
  conjs
   := reduce1(concatenate,
              map(method (sg) sg.blocked-conjunctions; end method,
                  reverse(proof-subgoal-agenda(*proof*))));
  conjs := concatenate(proof-blocked-conjunctions(*proof*), conjs);
  cl-find-if(forkable?, conjs);
end method best-blocked-conjunction;

// ----------------------------------------------------------------------------
define method forkable? (conjunction)
  // True IFF the conjunction could make progress by forking
  //    I.e.: If the current blocked conjunction got an answer, and then got no
  //    more answers (and so backtracked) after that, would there still be more
  //    in the conjunction space to search?
  if (instance?(conjunction, <dtp-forked-conjunction>))
    conjunction.stack-pointer > conjunction.top-conjunct;
  else
    conjunction.stack-pointer > 0;
  end if;
end method forkable?;

// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
nil(#f, nil(),
    "Return a copy of the conjunction (with some slot values copied)",
    nil(nil(#f, #f)),
    nil(nil(), nil(#f, nil(#())), nil(nil(#f, #()), nil(nil, nil(#f, #()))),
        nil(nil(#f, #()), nil(nil(#f, #()))),
        nil(nil(#(#(), #(), #(), #(), #(), #(), #(), #())),
            nil(nil(#f, #f), nil(#f, #f))),
        #f));

// ----------------------------------------------------------------------------
define method fork-specialize! (instance, original)
  for (conjunct in instance.list)
    conjunct.parent-conjunction := instance;
  end for;
  instance.top-conjunct := original.stack-pointer;
  let ac = #f;
  let subgoal = #f;
  ac := active-conjunct(instance);
  subgoal := ac.subgoal;
  let g15990 = ac;
  let g15987 = subgoal;
  let g15988 = #"conjuncts-to-propagate-to";
  let g15989 = add!(g15990, g15987.g15988);
  .inv-slot-value(g15987, g15988, g15989);
end method fork-specialize!;

// ----------------------------------------------------------------------------
"eof";

