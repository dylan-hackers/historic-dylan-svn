//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File gps1.lisp: First version of GPS (General Problem Solver)
// The current state: a list of conditions.
define variable *state* = #f;

// A list of available operators.
define variable *ops* = #f;

// An operation
define class <op> (<object>)
  slot op-action = #f, init-keyword: #"op-action";
  slot op-preconds = #f, init-keyword: #"op-preconds";
  slot op-add-list = #f, init-keyword: #"op-add-list";
  slot op-del-list = #f, init-keyword: #"op-del-list";
end class <op>;

define method gps (*state*, goals, *ops*)
  // General Problem Solver: achieve all goals using *ops*.
  if (every?(achieve, goals)) #"solved"; end if;
end method gps;

define method achieve (goal)
  // A goal is achieved if it already holds,
  //   or if there is an appropriate op for it that is applicable.
  member?(goal, *state*)
   | any?(apply-op, find-all(goal, *ops*, test: appropriate-p));
end method achieve;

define method appropriate-p (goal, op)
  // An op is appropriate to a goal if it is in its add list.
  member?(goal, op.op-add-list);
end method appropriate-p;

define method apply-op (op)
  // Print a message and update *state* if op is applicable.
  if (every?(achieve, op.op-preconds))
    print(list(#"executing", op.op-action), *standard-output*);
    *state* := set-difference(*state*, op.op-del-list);
    *state* := union(*state*, op.op-add-list);
    #t;
  end if;
end method apply-op;

//  ==============================
define variable *school-ops* =
  list(make-op(action: #"drive-son-to-school",
               preconds: #(#"son-at-home", #"car-works"),
               add-list: #(#"son-at-school"), del-list: #(#"son-at-home")),
       make-op(action: #"shop-installs-battery",
               preconds: #(#"car-needs-battery", #"shop-knows-problem",
                           #"shop-has-money"),
               add-list: #(#"car-works")),
       make-op(action: #"tell-shop-problem",
               preconds: #(#"in-communication-with-shop"),
               add-list: #(#"shop-knows-problem")),
       make-op(action: #"telephone-shop", preconds: #(#"know-phone-number"),
               add-list: #(#"in-communication-with-shop")),
       make-op(action: #"look-up-number", preconds: #(#"have-phone-book"),
               add-list: #(#"know-phone-number")),
       make-op(action: #"give-shop-money", preconds: #(#"have-money"),
               add-list: #(#"shop-has-money"), del-list: #(#"have-money")));

