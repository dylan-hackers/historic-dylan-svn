//  -*- Mode: Lisp; -*-
//  JSAINT: A rational reconstruction of Slagel's SAINT program
//  Last edited 1/29/92, by KDF
//  Copyright (c) 1991 -- 1992, Kenneth D. Forbus, Northwestern University,
//  and Johan de Kleer, Xerox Corporation
//  All rights reserved.
//  See the file legal.txt for a paragraph stating scope of permission
//  and disclaimer of warranty.  The above copyright notice and that
//  paragraph must be included in any separate copy of this file.
"(in-package common-lisp-user)";

define class <jsaint> (<object>)
  slot jsaint-title = "", init-keyword: #"jsaint-title";
  //  Name for printing
  slot jsaint-jtre = #f, init-keyword: #"jsaint-jtre";
  //  Associated JTRE
  slot jsaint-agenda = #f, init-keyword: #"jsaint-agenda";
  //  List of queued subproblems
  slot jsaint-problem = #f, init-keyword: #"jsaint-problem";
  //  When solved, we are done.
  slot jsaint-solution = #f, init-keyword: #"jsaint-solution";
  //  Cached answer.
  slot jsaint-n-subproblems = 0, init-keyword: #"jsaint-n-subproblems";
  //  Statistic
  slot jsaint-max-tasks = 20, init-keyword: #"jsaint-max-tasks";
  //  resource bound
  slot jsaint-debugging = #f, init-keyword: #"jsaint-debugging";
end class <jsaint>;

//  Debugging flag
//  Start with the usual encapsulation
#f;

define variable *jsaint* = #f;

define method create-jsaint (title, problem, #key debugging = #f,
                             max-tasks = #f)
  let ag
      = make-jsaint(title: title, problem: problem,
                    jtre: create-jtre(concatenate-as(<string>,
                                                     "JTRE of ",
                                                     title)),
                    debugging: debugging,
                    max-tasks: if (instance?(max-tasks, <integer>))
                                 max-tasks;
                               else
                                 20;
                               end if);
  in-jtre(ag.jsaint-jtre);
  change-jtms(jtre-jtms(ag.jsaint-jtre),
              contradiction-handler: jsaint-contradiction-handler);
  use-jsaint(ag);
end method create-jsaint;

// LTD: No macros.
#"debugging-jsaint";

define method change-jsaint (js, #key debugging = #"nada", problem = #"nada",
                             max-tasks = #"nada")
  if (~ (debugging == #"nada")) js.jsaint-debugging := debugging; end if;
  if (~ (problem == #"nada")) js.jsaint-problem := problem; end if;
  if (~ (max-tasks == #"nada")) js.jsaint-max-tasks := max-tasks; end if;
end method change-jsaint;

define method use-jsaint (js) *jsaint* := js; end method use-jsaint;

// LTD: No macros.
#"with-jsaint";

//  User entry point
define variable *jsaint-rules* =
  //  Fundamentals
  "/u/bps/code/jtms/jsrules.lisp";

define variable *jsaint-operators* =
  //  Operators
  "/u/bps/code/jtms/jsops.lisp";

define method solve-integral (integral,
                              #key title = as(<string>, generate-symbol()),
                              debugging = #f, max-tasks = 20)
  //  Remove redudancies and canonicalize input.
  integral
   := // LTD: Function EVAL not yet implemented.
      eval(quotize(simplifying-form-of(integral)));
  use-jsaint(create-jsaint(title, integral, debugging: debugging,
                           max-tasks: max-tasks));
  queue-problem(*jsaint*.jsaint-problem, #f);
  with-jtre(*jsaint*.jsaint-jtre,
            // LTD: Function LOAD not yet implemented.
            load(*jsaint-rules*),
            // LTD: Function LOAD not yet implemented.
            load(*jsaint-operators*));
  run-jsaint(*jsaint*);
end method solve-integral;

define method explain-result (#key *jsaint* = *jsaint*)
  if (empty?(*jsaint*.jsaint-solution))
    format-out("\n Problem not solved yet.");
  elseif (*jsaint*.jsaint-solution == #"failed-problem")
    explore-network(get-tms-node(bq-list(#"failed", *jsaint*.jsaint-problem),
                                 *jsaint*.jsaint-jtre));
    format-out("\n Failed to find a solution.");
  elseif (*jsaint*.jsaint-solution == #"failed-empty")
    format-out("\n Ran out of things to do.");
    explore-network(get-tms-node(bq-list(#"failed", *jsaint*.jsaint-problem),
                                 *jsaint*.jsaint-jtre));
  else
    format-out("\n Solved the problem:");
    explore-network(get-tms-node(bq-list(#"solution-of",
                                         *jsaint*.jsaint-problem,
                                         *jsaint*.jsaint-solution),
                                 *jsaint*.jsaint-jtre));
  end if;
end method explain-result;

//  Basic algorithm
define method run-jsaint (*jsaint*)
  block (return-from-run-jsaint)
    if (*jsaint*.jsaint-solution)
      return-from-run-jsaint(*jsaint*.jsaint-solution, *jsaint*);
    end if;
    if (*jsaint*.jsaint-n-subproblems > *jsaint*.jsaint-max-tasks)
      return-from-run-jsaint(time-out: *jsaint*);
    end if;
    for (done? = nil then nil,
         solution = fetch-solution(*jsaint*.jsaint-problem,
                                   *jsaint*) then fetch-solution(*jsaint*
                                                                 .jsaint-problem,
                                                                 *jsaint*),
         failure-signal = backquote(failed(integrate(bq-comma(*jsaint*
                                                              .jsaint-problem)))) then backquote(failed(integrate(bq-comma(*jsaint*
                                                                                                                           .jsaint-problem)))),
         until done?)
      if (solution)
        *jsaint*.jsaint-solution := solution;
        debugging-jsaint(*jsaint*, "~% ~A: Solved original problem.",
                         *jsaint*.jsaint-title);
        done? := #t;
      elseif (in?(failure-signal, *jsaint*.jsaint-jtre))
        debugging-jsaint(*jsaint*, "~% ~A: Failed on original problem.",
                         *jsaint*.jsaint-title);
        *jsaint*.jsaint-solution := #"failed-problem";
        done? := #t;
      elseif (empty?(*jsaint*.jsaint-agenda))
        debugging-jsaint(*jsaint*, "~% ~A: Agenda empty.",
                         *jsaint*.jsaint-title);
        *jsaint*.jsaint-solution := #"failed-empty";
        done? := #t;
      else
        process-subproblem(tail(pop!(*jsaint*.jsaint-agenda)));
      end if;
    finally
      values(*jsaint*.jsaint-solution, *jsaint*);
    end for;
  end block;
end method run-jsaint;

define method process-subproblem (item)
  block (return-from-process-subproblem)
    debugging-jsaint(*jsaint*, "~%  Trying to solve ~A.", item);
    open-subproblem(item);
    if (fetch-solution(item, *jsaint*))
      //  Bookkeeping is done by pdis rules
      debugging-jsaint(*jsaint*, "~%    ..already solved.");
      return-from-process-subproblem(#t);
    end if;
    if (any?(method (f) in?(f, jtre); end method, //  Already expanded
             fetch(bq-list*(#"and-subgoals", item, #(#"?subproblems")),
                   jtre)))
      debugging-jsaint(*jsaint*, "~%   ..already expanded.");
      return-from-process-subproblem(#t);
    end if;
    for (suggestion in fetch(bq-list*(#"suggest-for", item, #(#"?operator")),
                             jtre))
      if (in?(suggestion, jtre))
        queue-problem(bq-list(#"try", third(suggestion)), item);
        push!(bq-list(#"try", third(suggestion)), suggestions);
      end if;
    end for;
    //  Presume extra subgoals don't come along.
    assert!(bq-list(#"or-subgoals", item, suggestions), or-subgoals: jtre);
    run-rules(jtre);
  end block;
end method process-subproblem;

define method open-subproblem (item)
  assert!(bq-list(#"expanded", item), expand-agenda-item: jtre);
  assume!(bq-list(#"open", item), expand-agenda-item: jtre);
  //  Look for quick win, extra consequences.
  run-rules(jtre);
end method open-subproblem;

//  Queuing problems
//  Queue entries take the form (<difficulty> . <subproblem>)
//  Difficulty estimates are based on the form of the subproblem
//  alone, since there could be multiple parents for a subproblem.
define method queue-problem (problem, parent)
  entry := pair(estimate-difficulty(problem), problem);
  debugging-jsaint(*jsaint*, "~%   Queueing ~A, difficulty = ~D", problem,
                   head(entry));
  *jsaint*.jsaint-agenda
   := cl-merge(<list>, list(entry), *jsaint*.jsaint-agenda,
               method (a, b) head(a) < head(b); end method);
end method queue-problem;

define method estimate-difficulty (problem)
  max-depth(problem) + count-symbols(problem);
end method estimate-difficulty;

define method count-symbols (pr)
  if (empty?(pr))
    0;
  elseif (instance?(pr, <list>))
    reduce(\+, 0, map(count-symbols, pr));
  else
    1;
  end if;
end method count-symbols;

define method max-depth (pr)
  if (~ instance?(pr, <list>))
    1;
  else
    reduce(max, 0, map(max-depth, pr)) + 1;
  end if;
end method max-depth;

//  Auxiliary routines
define method fetch-solution (problem, #key *jsaint* = *jsaint*)
  block (return-from-fetch-solution)
    for (solution in fetch(bq-list*(#"solution-of", problem, #(#"?answer")),
                           jtre))
      if (in?(solution, jtre))
        return-from-fetch-solution(third(solution));
      end if;
    end for;
  end block;
end method fetch-solution;

define method jsaint-contradiction-handler (contradictions, jtms)
  ask-user-hander(contradictions, jtms);
end method jsaint-contradiction-handler;

//  default
//  Defining operators
// LTD: No macros.
#"defintegration";

define variable *test-operator* =
  #(#"defintegration", #"integral-of-sum",
    #(#"integral", #(#"+", #"?t1", #"?t2"), #"?var"), #"subproblems",
    #(#(#"?int1", #(#"integrate", #(#"integral", #"?t1", #"?var"))),
      #(#"?int2", #(#"integrate", #(#"integral", #"?t2", #"?var")))),
    #"result", #(#"+", #"?int1", #"?int2"));

//  Helpers for operator definition
define method calculate-subproblem-list (subproblems)
  //  Takes list of entries whose form is (?result-var ?form)
  //  and returns a list of (?goal-var ?form)
  map(method (pair)
        inc!(counter);
        list(as(<symbol>, format(#f, "?GOAL%D", counter)),
             simplifying-form-of(second(pair)));
      end method,
      subproblems);
end method calculate-subproblem-list;

define method simplifying-form-of (alg-goal)
  //  Run simplifier on subgoals, just in case.
  if (empty?(alg-goal))
    #f;
  elseif (~ instance?(alg-goal, <list>))
    alg-goal;
  elseif (head(alg-goal) == #"integral")
    //  Simplify as needed
    bq-list(#"integral",
            bq-list(#"eval", bq-list(#"simplify", quotize(second(alg-goal)))),
            third(alg-goal));
  else
    pair(simplifying-form-of(head(alg-goal)),
         simplifying-form-of(tail(alg-goal)));
  end if;
end method simplifying-form-of;

define method calculate-solution-rule-parts (sub-pairs, res-pairs)
  triggers
   := map(method (subpair, respair)
            inc!(counter);
            let rvar = as(<symbol>, format(#f, "?RESULT%D", counter));
            push!(rvar, antes);
            bq-list(#"in",
                    bq-list(#"solution-of", head(subpair), head(respair)),
                    #"var", rvar);
          end method,
          sub-pairs, res-pairs);
  values(triggers, reverse!(antes));
end method calculate-solution-rule-parts;

define method keywordize (stuff)
  if (empty?(stuff))
    error("Can't keywordize nothing.");
  elseif (instance?(stuff, <list>))
    keywordize(head(stuff));
  else
    as(<symbol>, format(#f, "%S", stuff));
  end if;
end method keywordize;

//  Interrogatives
//  SHOW-PROBLEM highlights the assertions relevant to
//  the given problem.
define method show-problem (pr, #key *jsaint* = *jsaint*)
  format-out("\n%S:: (%D)", pr, estimate-difficulty(pr));
  with-jtre(*jsaint*.jsaint-jtre,
            stuff := fetch(bq-list*(#"parent-of", pr, #(#"?x", #"?type"))),
            if (stuff)
              format-out("\n Parent(s): ");
              for (p in stuff)
                if (in?(p))
                  format-out("\n   %S, %S.", third(p), p[3]);
                else
                  format-out("\n    BUG: Should be in: %S", p);
                end if;
              end for;
            else
              format-out("\n No parents found.");
            end if,
            if (fetch(bq-list(#"expanded", pr)))
              format-out("\n Expanded,");
            else
              format-out("\n Not expanded,");
            end if,
            if (fetch(bq-list(#"open", pr)))
              if (in?(bq-list(#"open", pr)))
                format-out(" open,");
              else
                format-out(" closed,");
              end if;
            else
              format-out(" not opened,");
            end if,
            if (in?(bq-list(#"relevant", pr)))
              format-out(" relevant.");
            else
              format-out(" not relevant.");
            end if,
            if (stuff := fetch-solution(pr))
              format-out("\n Solved, solution = %S", stuff);
            elseif ((stuff := head(fetch(bq-list(#"failed", pr))))
                     & in?(stuff))
              format-out("\n  Failed.");
            elseif (~ (head(pr) = #"try"))
              format-out("\n Neither solved nor failed.");
            end if,
            ands := fetch(bq-list*(#"and-subgoals", pr, #(#"?ands"))),
            if (ands)
              format-out("\n And subgoals:");
              for (subg in third(head(ands)))
                format-out("\n   %S", subg);
              end for;
              format-out(".");
            end if,
            ors := fetch(bq-list*(#"or-subgoals", pr, #(#"?ors"))),
            if (ors)
              format-out("\n Or subgoals:");
              for (subg in third(head(ors)))
                format-out("\n   %S", subg);
              end for;
              format-out(".");
            end if);
end method show-problem;

//  Textual display of an AND/OR graph
define method show-ao-graph (#key *jsaint* = *jsaint*)
  let problems = get-problems();
  let depth-table
      = update-ao-depth-table(*jsaint*.jsaint-problem, 0,
                              list(pair(*jsaint*.jsaint-problem, 0)),
                              list(*jsaint*.jsaint-problem));
  depth-table
   := sort!(depth-table, test: method (x, y) tail(x) < tail(y); end method);
  for (pair in depth-table)
    format-out("\n %D:", tail(pair));
    show-problem(head(pair));
  end for;
end method show-ao-graph;

define method update-ao-depth-table (now, depth, depths, path)
  inc!(depth);
  for (child in get-children(now))
    if (~ member?(child, path, test: #"equal"))
      let entry = cl-assoc(child, depths, test: #"equal");
      if (~ entry) push!(entry := pair(child, 0), depths); end if;
      if (depth > tail(entry))
        tail(entry) := depth;
        depths
         := update-ao-depth-table(child, depth, depths, pair(child, path));
      end if;
    end if;
  finally
    depths;
  end for;
end method update-ao-depth-table;

define method get-children (gp, #key *jsaint* = *jsaint*)
  for (maybe-kid in fetch(bq-list*(#"parent-of", #"?x", gp, #(#"?type")),
                          *jsaint*.jsaint-jtre))
    if (in?(maybe-kid, *jsaint*.jsaint-jtre))
      push!(second(maybe-kid), children);
    end if;
  finally
    children;
  end for;
end method get-children;

define method get-problems (#key *jsaint* = *jsaint*)
  map(second, fetch(#(#"expanded", #"?x"), *jsaint*.jsaint-jtre));
end method get-problems;

//  Debugging
define method try-jsaint (problem, #key title = "JSAINT Test")
  solve-integral(problem, debugging: #t, title: title);
end method try-jsaint;

define method jfetch (pattern)
  fetch(pattern, *jsaint*.jsaint-jtre);
end method jfetch;

define variable problem1 = #(#"integrate", #(#"integral", 1, #"x"));

define variable problem2 =
  #(#"integrate", #(#"integral", #(#"+", #"x", 5), #"x"));

define variable problem3 =
  #(#"integrate", #(#"integral", #(#"*", 46, #(#"log", #"x", #"%e")), #"x"));

problem4
 := #(#"integrate",
      #(#"integral",
        #(#"+", 0.63, #(#"*", 3.2, #(#"sin", #(#"*", 1.7, #"x"))),
          #(#"*", 4, #(#"expt", #"%e", #(#"*", 2, #"x")))),
        #"x"));

