//  -*- Mode: Lisp; Syntax: Common-Lisp;  -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  krep1.lisp: Knowledge representation code; second version.
//  Fixes problem with renaming variables; adds conjunctions.
requires("krep1");

//  Need some functions from previous version
define method index (key)
  // Store key in a dtree node.  Key must be (predicate . args);
  //   it is stored in the predicate's dtree.
  dtree-index(key, rename-variables(key), //  store unique vars
              get-dtree(predicate(key)));
end method index;

//  ==============================
//  The following iterated-deepening code is not used, but is
//  included for those who want to incorporate it into prolog.
// Has the search been stopped?
define variable *search-cut-off* = #f;

define method prove-all (goals, bindings, depth)
  // Find a solution to the conjunction of goals.
  //  This version just passes the depth on to PROVE.
  if (bindings == fail)
    fail;
  elseif (empty?(goals))
    bindings;
  else
    prove(first(goals), bindings, tail(goals), depth);
  end if;
end method prove-all;

define method prove (goal, bindings, other-goals, depth)
  // Return a list of possible solutions to goal.
  //  Check if the depth bound has been exceeded
  if (depth = 0)
    *search-cut-off* := #t;
    // ***
    fail;
  else
    let clauses = get-clauses(predicate(goal));
    if (instance?(clauses, <list>))
      any?(method (clause)
             let new-clause = rename-variables(clause);
             prove-all(concatenate(clause-body(new-clause), other-goals),
                       unify(goal, clause-head(new-clause), bindings),
                       depth - 1);
           end method,
           // ***
           clauses);
    else
      //  The predicate's "clauses" can be an atom:
      //  a primitive function to call
      clauses(tail(goal), bindings, other-goals, depth);
    end if;
  end if;
end method prove;

// ***
//  ==============================
// The depth of the first round of iterative search.
define variable *depth-start* = 5;

// Increase each iteration of the search by this amount.
define variable *depth-incr* = 5;

// The deepest we will ever search.
define variable *depth-max* = $most-positive-fixnum;

//  ==============================
define method top-level-prove (goals)
  let all-goals
      = concatenate(goals,
                    list(pair(#"show-prolog-vars", variables-in(goals))));
  for (depth from *depth-start* to *depth-max* by *depth-incr*,
       while begin
               fluid-bind (*search-cut-off* = #f)
                 prove-all(all-goals, no-bindings, depth);
                 *search-cut-off*;
               end fluid-bind;
             end);
  end for;
  format-out("\nNo.");
  values();
end method top-level-prove;

//  ==============================
define method show-prolog-vars (vars, bindings, other-goals, depth)
  // Print each variable with its binding.
  //   Then ask the user if more solutions are desired.
  if (depth > *depth-incr*)
    fail;
  else
    if (empty?(vars))
      format-out("\nYes");
    else
      for (var in vars)
        format-out("\n%S = %S", var, subst-bindings(bindings, var));
      end for;
    end if;
    if (continue-p())
      fail;
    else
      prove-all(other-goals, bindings, depth);
    end if;
  end if;
end method show-prolog-vars;

//  ==============================
//  Adding support for conjunctions:
define method add-fact (fact)
  // Add the fact to the data base.
  if (predicate(fact) == #"and")
    let list92543 = args(fact);
    begin do(add-fact, list92543); list92543; end;
  else
    index(fact);
  end if;
end method add-fact;

//  ==============================
define method retrieve-fact (query, #key bindings = no-bindings)
  // Find all facts that match query.  Return a list of bindings.
  if (predicate(query) == #"and")
    retrieve-conjunction(args(query), list(bindings));
  else
    retrieve(query, bindings);
  end if;
end method retrieve-fact;

define method retrieve-conjunction (conjuncts, bindings-lists)
  // Return a list of binding lists satisfying the conjuncts.
  apply(concatenate!,
        map(method (bindings)
              if (bindings == fail)
                #f;
              elseif (empty?(conjuncts))
                list(bindings);
              else
                retrieve-conjunction(tail(conjuncts),
                                     retrieve-fact(subst-bindings(bindings,
                                                                  first(conjuncts)),
                                                   bindings));
              end if;
            end method,
            bindings-lists));
end method retrieve-conjunction;

//  ==============================
define method mapc-retrieve (fn, query, #key bindings = no-bindings)
  // For every fact that matches the query,
  //   apply the function to the binding list.
  for (bucket in fetch(query))
    for (answer in bucket)
      let new-bindings = unify(query, answer, bindings);
      if (~ (new-bindings == fail)) fn(new-bindings); end if;
    end for;
  end for;
end method mapc-retrieve;

define method retrieve (query, #key bindings = no-bindings)
  // Find all facts that match query.  Return a list of bindings.
  let answers = #f;
  mapc-retrieve(method (bindings) push!(bindings, answers); end method, query,
                bindings);
  answers;
end method retrieve;

//  ==============================
define method retrieve-bagof (query)
  // Find all facts that match query.
  //   Return a list of queries with bindings filled in.
  map(method (bindings) subst-bindings(bindings, query); end method,
      retrieve-fact(query));
end method retrieve-bagof;

define method retrieve-setof (query)
  // Find all facts that match query.
  //   Return a list of unique queries with bindings filled in.
  cl-remove-duplicates(retrieve-bagof(query), test: \=);
end method retrieve-setof;

//  ==============================
//  Get ready for attached functions in the next version:
// LTD: No macros.
#"def-attached-fn";

