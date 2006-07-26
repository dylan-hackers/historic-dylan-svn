//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Structures.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
define class <proof> (<object>)
  //  Specified by user
  slot proof-query, init-keyword: #"proof-query";
  slot proof-theory = *theory*, init-keyword: #"proof-theory";
  slot proof-return-form = #f, init-keyword: #"proof-return-form";
  //  NIL => plug in to original query
  slot proof-sda = #f, init-keyword: #"proof-sda";
  //  If true, suppress disjunctive answers
  //  Internal
  slot proof-query-conjunctions, init-keyword: #"proof-query-conjunctions";
  slot proof-blocked-conjunctions = #f,
       init-keyword: #"proof-blocked-conjunctions";
  slot proof-subgoal-agenda, init-keyword: #"proof-subgoal-agenda";
  slot proof-new-answers = #f, init-keyword: #"proof-new-answers";
  //  Acquired by propagation, will move...
  slot proof-answers = #f, init-keyword: #"proof-answers";
  //  ...to here once processed
  slot proof-goal-nodes, init-keyword: #"proof-goal-nodes";
  slot proof-pure-literal-nodes = make(<table>, test: \==),
       init-keyword: #"proof-pure-literal-nodes";
  //  Cache for kb nodes to ignore
  slot proof-assumables = map(list-to-literal, *assumables*),
       init-keyword: #"proof-assumables";
  //  List of LITERAL-NODEs to match
  slot proof-consistency-check = *consistency-check*,
       init-keyword: #"proof-consistency-check";
  //  Caching
  slot proof-subgoal-index, init-keyword: #"proof-subgoal-index";
  //  :recursion, :postponement
  slot proof-subgoal-cache = #f, init-keyword: #"proof-subgoal-cache";
  //  :subgoals
  slot proof-success-cache = #f, init-keyword: #"proof-success-cache";
  //  :success, :answers
  slot proof-failure-cache = #f, init-keyword: #"proof-failure-cache";
  //  :failure, :answers
  slot proof-cache-count = 0, init-keyword: #"proof-cache-count";
  //  Number of items in the cache
  //  Iteration and cutoffs
  slot proof-subgoal-depth-cutoff = *initial-subgoal-depth*,
       init-keyword: #"proof-subgoal-depth-cutoff";
  slot proof-subgoal-depth-skip = *subgoal-depth-skip*,
       init-keyword: #"proof-subgoal-depth-skip";
  slot proof-subgoal-maximum-depth = *subgoal-maximum-depth*,
       init-keyword: #"proof-subgoal-maximum-depth";
  slot proof-subgoal-cutoff-occurred,
       init-keyword: #"proof-subgoal-cutoff-occurred";
  slot proof-function-depth-cutoff = *initial-function-depth*,
       init-keyword: #"proof-function-depth-cutoff";
  slot proof-function-depth-skip = *function-depth-skip*,
       init-keyword: #"proof-function-depth-skip";
  slot proof-function-maximum-depth = *function-maximum-depth*,
       init-keyword: #"proof-function-maximum-depth";
  slot proof-function-cutoff-occurred,
       init-keyword: #"proof-function-cutoff-occurred";
end class <proof>;

define method print-proof (structure, stream, depth)
  (method (s, #rest args)
     apply(maybe-initiate-xp-printing,
           method (xp, #rest args)
             let init = args;
             begin
               write-string++("#<Proof of ", xp, 0, 11);
               fluid-bind (*print-escape* = #f)
                 write+(pop!(args), xp);
               end fluid-bind;
               write-string++(" with ", xp, 0, 6);
               using-format(xp, "~D", pop!(args));
               write-string++(" answer", xp, 0, 7);
               if (~ (head(backup-in-list(1, init, args)) == 1))
                 write-char++('s', xp);
               end if;
             end;
             if (args) copy-sequence(args); end if;
           end method,
           s, args);
   end method)(stream, structure.proof-query, size(structure.proof-answers));
  if (empty?(structure.proof-query-conjunctions))
    format(stream, " [Complete]");
  end if;
  format(stream, ">");
end method print-proof;

// ----------------------------------------------------------------------------
define class <answer> (<object>)
  slot answer-binding-list = #f, init-keyword: #"answer-binding-list";
  slot answer-context = #f, init-keyword: #"answer-context";
  //  List (possibly empty) of reduction subgoals
  slot answer-label = #f, init-keyword: #"answer-label";
  slot answer-residue = #f, init-keyword: #"answer-residue";
  slot answer-ae-binding-lists = #f, init-keyword: #"answer-ae-binding-lists";
  //  Disjunctive answers
  slot answer-subgoal = #f, init-keyword: #"answer-subgoal";
end class <answer>;

define method print-answer (structure, stream, depth)
  if (structure.answer-context)
    format(stream, "#<R answer [");
    for (subgoal in reverse(structure.answer-context), first = %t then %f)
      if (~ first) format(stream, ","); end if;
      print-literal-node(subgoal.literal, s: stream);
    end for;
    format(stream, "]");
  else
    format(stream, "#<Answer");
  end if;
  if (structure.answer-binding-list)
    print-binding-list(structure.answer-binding-list, s: stream);
  else
    format(stream, " TRUE");
  end if;
  if (structure.answer-label)
    format(stream, " with label %S", structure.answer-label.label-value);
  end if;
  if (structure.answer-residue)
    format(stream, " with residue");
    for (literal in structure.answer-residue)
      format(stream, " ");
      print-literal-node(literal, s: stream);
    end for;
  end if;
  for (ae-bl in structure.answer-ae-binding-lists)
    format(stream, " or");
    print-binding-list(ae-bl, s: stream);
  end for;
  format(stream, ">");
end method print-answer;

// ----------------------------------------------------------------------------
define class <kb-node> (<object>)
  slot kb-node-id, init-keyword: #"kb-node-id";
  slot kb-node-clause = #f, init-keyword: #"kb-node-clause";
end class <kb-node>;

define method print-kb-node (structure, stream, depth)
  format(stream, "#<%S:%S>", structure.kb-node-id, structure.kb-node-clause);
end method print-kb-node;

// ----------------------------------------------------------------------------
define class <justification> (<object>); end class <justification>;

define class <l-justification> (<justification>)
  slot l-just-id, init-keyword: #"l-just-id";
end class <l-justification>;

define class <s-justification> (<justification>)
  slot s-just-conjunct-number, init-keyword: #"s-just-conjunct-number";
  slot s-just-subgoal, init-keyword: #"s-just-subgoal";
  slot s-just-answer, init-keyword: #"s-just-answer";
  slot s-just-t-bl, init-keyword: #"s-just-t-bl";
end class <s-justification>;

define class <c-justification> (<justification>)
  slot c-just-conjunction, init-keyword: #"c-just-conjunction";
  slot c-just-answer, init-keyword: #"c-just-answer";
end class <c-justification>;

define class <res-justification> (<justification>)
  slot res-just-assumable, init-keyword: #"res-just-assumable";
end class <res-justification>;

define class <r-justification> (<justification>)
  slot r-just-ancestor-subgoal, init-keyword: #"r-just-ancestor-subgoal";
  slot r-just-leaf-subgoal, init-keyword: #"r-just-leaf-subgoal";
end class <r-justification>;

// ----------------------------------------------------------------------------
define class <cache-justification> (<justification>);
end class <cache-justification>;

define class <s-cache-justification> (<cache-justification>)
  slot s-cache-just-literal, init-keyword: #"s-cache-just-literal";
end class <s-cache-justification>;

define class <f-cache-justification> (<cache-justification>)
  slot f-cache-just-literal, init-keyword: #"f-cache-just-literal";
end class <f-cache-justification>;

define class <sg-cache-justification> (<cache-justification>)
  slot sg-cache-just-subgoal, init-keyword: #"sg-cache-just-subgoal";
end class <sg-cache-justification>;

// ----------------------------------------------------------------------------
"eof";

