//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File gps.lisp: Final version of GPS
requires("gps1");

//  ==============================
define method executing-p (x)
  // Is x of the form: (executing ...) ?
  starts-with(x, #"executing");
end method executing-p;

define method starts-with (list, x)
  // Is this a list whose first element is x?
  instance?(list, <pair>) & first(list) == x;
end method starts-with;

define method convert-op (op)
  // Make op conform to the (EXECUTING op) convention.
  if (~ any?(executing-p, op-add-list(op)))
    push!(list(#"executing", op-action(op)), op-add-list(op));
  end if;
  op;
end method convert-op;

define method op (action, #key preconds, add-list, del-list)
  // Make a new operator that obeys the (EXECUTING op) convention.
  convert-op(make-op(action: action, preconds: preconds, add-list: add-list,
                     del-list: del-list));
end method op;

//  ==============================
begin do(convert-op, *school-ops*); *school-ops*; end;

//  ==============================
// A list of available operators.
define variable *ops* = #f;

// An operation
define class <op> (<object>)
  slot op-action = #f, init-keyword: #"op-action";
  slot op-preconds = #f, init-keyword: #"op-preconds";
  slot op-add-list = #f, init-keyword: #"op-add-list";
  slot op-del-list = #f, init-keyword: #"op-del-list";
end class <op>;

define method gps (state, goals, #key *ops* = *ops*)
  // General Problem Solver: from state, achieve goals using *ops*.
  choose(complement(method (x) not(instance?(x, <list>)); end method),
         achieve-all(pair(#(#"start"), state), goals, #f));
end method gps;

//  ==============================
define method achieve-all (state, goals, goal-stack)
  // Achieve each goal, and make sure they still hold at the end.
  let current-state = state;
  if (every?(method (g)
               (current-state := achieve(current-state, g, goal-stack));
             end method,
             goals)
       & subset?(goals, current-state, test: \=))
    current-state;
  end if;
end method achieve-all;

define method achieve (state, goal, goal-stack)
  // A goal is achieved if it already holds,
  //   or if there is an appropriate op for it that is applicable.
  dbg-indent(gps: size(goal-stack), "Goal: ~a", goal);
  if (member-equal(goal, state))
    state;
  elseif (member-equal(goal, goal-stack))
    #f;
  else
    any?(method (op) apply-op(state, goal, op, goal-stack); end method,
         find-all(goal, *ops*, test: appropriate-p));
  end if;
end method achieve;

//  ==============================
define method member-equal (item, list)
  member?(item, list, test: \=);
end method member-equal;

//  ==============================
define method apply-op (state, goal, op, goal-stack)
  // Return a new, transformed state if op is applicable.
  dbg-indent(gps: size(goal-stack), "Consider: ~a", op.op-action);
  let state2 = achieve-all(state, op.op-preconds, pair(goal, goal-stack));
  if (~ empty?(state2))
    //  Return an updated state
    dbg-indent(gps: size(goal-stack), "Action: ~a", op.op-action);
    concatenate(choose(complement(method (x)
                                    member-equal(x, op.op-del-list);
                                  end method),
                       state2),
                op.op-add-list);
  end if;
end method apply-op;

define method appropriate-p (goal, op)
  // An op is appropriate to a goal if it is in its add list.
  member-equal(goal, op.op-add-list);
end method appropriate-p;

//  ==============================
define method use (oplist)
  // Use oplist as the default list of operators.
  //  Return something useful, but not too verbose: 
  //  the number of operators.
  size(*ops* := oplist);
end method use;

//  ==============================
define variable *banana-ops* =
  list(op(#"climb-on-chair",
          preconds: #(#"chair-at-middle-room", #"at-middle-room",
                      #"on-floor"),
          add-list: #(#"at-bananas", #"on-chair"),
          del-list: #(#"at-middle-room", #"on-floor")),
       op(#"push-chair-from-door-to-middle-room",
          preconds: #(#"chair-at-door", #"at-door"),
          add-list: #(#"chair-at-middle-room", #"at-middle-room"),
          del-list: #(#"chair-at-door", #"at-door")),
       op(#"walk-from-door-to-middle-room",
          preconds: #(#"at-door", #"on-floor"),
          add-list: #(#"at-middle-room"), del-list: #(#"at-door")),
       op(#"grasp-bananas", preconds: #(#"at-bananas", #"empty-handed"),
          add-list: #(#"has-bananas"), del-list: #(#"empty-handed")),
       op(#"drop-ball", preconds: #(#"has-ball"),
          add-list: #(#"empty-handed"), del-list: #(#"has-ball")),
       op(#"eat-bananas", preconds: #(#"has-bananas"),
          add-list: #(#"empty-handed", #"not-hungry"),
          del-list: #(#"has-bananas", #"hungry")));

//  ==============================
define method make-maze-ops (pair)
  // Make maze ops in both directions
  list(make-maze-op(first(pair), second(pair)),
       make-maze-op(second(pair), first(pair)));
end method make-maze-ops;

define method make-maze-op (here, there)
  // Make an operator to move between two places
  op(list(#"move", #"from", here, #"to", there),
     preconds: list(list(#"at", here)), add-list: list(list(#"at", there)),
     del-list: list(list(#"at", here)));
end method make-maze-op;

define variable *maze-ops* =
  mappend(make-maze-ops,
          #(#(1, 2), #(2, 3), #(3, 4), #(4, 9), #(9, 14), #(9, 8), #(8, 7),
            #(7, 12), #(12, 13), #(12, 11), #(11, 6), #(11, 16), #(16, 17),
            #(17, 22), #(21, 22), #(22, 23), #(23, 18), #(23, 24), #(24, 19),
            #(19, 20), #(20, 15), #(15, 10), #(10, 5), #(20, 25)));

//  ==============================
define method gps (state, goals, #key *ops* = *ops*)
  // General Problem Solver: from state, achieve goals using *ops*.
  find-all-if(action-p, achieve-all(pair(#(#"start"), state), goals, #f));
end method gps;

define method action-p (x)
  // Is x something that is (start) or (executing ...)?
  x = #(#"start") | executing-p(x);
end method action-p;

//  ==============================
define method find-path (start, end)
  // Search a maze for a path from start to end.
  let results = gps(list(list(#"at", start)), list(list(#"at", end)));
  if (~ empty?(results))
    pair(start, map(destination, remove(results, #(#"start"), test: \=)));
  end if;
end method find-path;

define method destination (action)
  // Find the Y in (executing (move from X to Y))
  second(action)[4];
end method destination;

//  ==============================
define method make-block-ops (blocks)
  let ops = #f;
  for (a in blocks)
    for (b in blocks)
      if (~ (a = b))
        for (c in blocks)
          if (~ (c = a | c = b)) push!(move-op(a, b, c), ops); end if;
        end for;
        push!(move-op(a, #"table", b), ops);
        push!(move-op(a, b, #"table"), ops);
      end if;
    end for;
  end for;
  ops;
end method make-block-ops;

define method move-op (a, b, c)
  // Make an operator to move A from B to C.
  op(list(#"move", a, #"from", b, #"to", c),
     preconds: list(list(#"space", #"on", a), list(#"space", #"on", c),
                    list(a, #"on", b)),
     add-list: move-ons(a, b, c), del-list: move-ons(a, c, b));
end method move-op;

define method move-ons (a, b, c)
  if (b == #"table")
    list(list(a, #"on", c));
  else
    list(list(a, #"on", c), list(#"space", #"on", b));
  end if;
end method move-ons;

//  ==============================
define method achieve-all (state, goals, goal-stack)
  // Achieve each goal, trying several orderings.
  any?(method (goals) achieve-each(state, goals, goal-stack); end method,
       orderings(goals));
end method achieve-all;

define method achieve-each (state, goals, goal-stack)
  // Achieve each goal, and make sure they still hold at the end.
  let current-state = state;
  if (every?(method (g)
               (current-state := achieve(current-state, g, goal-stack));
             end method,
             goals)
       & subset?(goals, current-state, test: \=))
    current-state;
  end if;
end method achieve-each;

define method orderings (l)
  if (size(l) > 1) list(l, reverse(l)); else list(l); end if;
end method orderings;

//  ==============================
define method achieve (state, goal, goal-stack)
  // A goal is achieved if it already holds,
  //   or if there is an appropriate op for it that is applicable.
  dbg-indent(gps: size(goal-stack), "Goal: ~a", goal);
  if (member-equal(goal, state))
    state;
  elseif (member-equal(goal, goal-stack))
    #f;
  else
    any?(method (op) apply-op(state, goal, op, goal-stack); end method,
         appropriate-ops(goal, state));
  end if;
end method achieve;

// ***
define method appropriate-ops (goal, state)
  // Return a list of appropriate operators, 
  //   sorted by the number of unfulfilled preconditions.
  sort!(copy-sequence(find-all(goal, *ops*, test: appropriate-p)),
        test: method (x, y)
                (method (op)
                   cl-count-if(method (precond)
                                 ~ member-equal(precond, state);
                               end method,
                               op.op-preconds);
                 end method)(x)
                 < (method (op)
                      cl-count-if(method (precond)
                                    ~ member-equal(precond, state);
                                  end method,
                                  op.op-preconds);
                    end method)(y);
              end method);
end method appropriate-ops;

//  ==============================
define method permutations (bag)
  // Return a list of all the permutations of the input.
  //  If the input is nil, there is only one permutation:
  //  nil itself
  if (empty?(bag))
    #(#"()");
  else
    //  Otherwise, take an element, e, out of the bag.
    //  Generate all permutations of the remaining elements,
    //  And add e to the front of each of these.
    //  Do this for all possible e to generate all permutations.
    apply(concatenate!,
          map(method (e)
                map(method (p) pair(e, p); end method,
                    permutations(remove(bag, e, count: 1)));
              end method,
              bag));
  end if;
end method permutations;

//  ==============================
"eof";

