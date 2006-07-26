//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File unifgram.lisp: The DCG parser from Chapter 20.
requires("prologcp");

// LTD: No macros.
#"rule";

symbol-get-property(#"-", #"rule-function")
 := method (head, body) apply(list, #"<-", head, body); end method;

define method dcg-normal-goal-p (x)
  starts-with(x, #"test") | x == #"!";
end method dcg-normal-goal-p;

define method dcg-word-list-p (x)
  starts-with(x, #"word");
end method dcg-word-list-p;

symbol-get-property(#"-->", #"rule-function") := #"make-dcg";

define method make-dcg (head, body)
  let n = cl-count-if(complement(dcg-normal-goal-p), body);
  apply(list, #"<-", concatenate(head, list(#"?s0", symbol(#"?s", n))),
        make-dcg-body(body, 0));
end method make-dcg;

define method make-dcg-body (body, n)
  // Make the body of a Definite Clause Grammar (DCG) clause.
  //   Add ?string-in and -out variables to each constituent.
  //   Goals like (:test goal) are ordinary Prolog goals,
  //   and goals like (:word hello) are literal words to be parsed.
  if (empty?(body))
    #f;
  else
    let goal = first(body);
    if (goal == #"!")
      pair(#"!", make-dcg-body(tail(body), n));
    elseif (dcg-normal-goal-p(goal))
      concatenate(tail(goal), make-dcg-body(tail(body), n));
    elseif (dcg-word-list-p(goal))
      pair(list(#"=", symbol(#"?s", n),
                concatenate(tail(goal), symbol(#"?s", n + 1))),
           make-dcg-body(tail(body), n + 1));
    else
      pair(concatenate(goal, list(symbol(#"?s", n), symbol(#"?s", n + 1))),
           make-dcg-body(tail(body), n + 1));
    end if;
  end if;
end method make-dcg-body;

symbol-get-property(#"==>", #"rule-function") := #"make-augmented-dcg";

define method make-augmented-dcg (head, body)
  // Build an augmented DCG rule that handles :sem, :ex,
  //   and automatic conjunctiontive constituents.
  if (last1(head) == #"sem")
    let ?sem = generate-symbol(#"string"("?SEM"));
    make-augmented-dcg(concatenate(copy-sequence(head, size(head) - 1),
                                   list(?sem)),
                       concatenate(remove(body,
                                          sem: #"test",
                                          method (x, y)
                                          x == first-or-nil(y);
                                          end method),
                                   list(list(#"test",
                                             collect-sems(body, ?sem)))));
  else
    //  Separate out examples from body
    let (exs, new-body)
        = partition-if(method (x) starts-with(x, #"ex"); end method, body);
    let rule = apply(list, #"rule", handle-conj(head), #"-->", new-body);
    if (empty?(exs))
      rule;
    else
      list(#"progn", apply(list, #"ex", head, mappend(tail, exs)), rule);
    end if;
  end if;
end method make-augmented-dcg;

define method collect-sems (body, ?sem)
  // Get the semantics out of each constituent in body,
  //   and combine them together into ?sem.
  let sems
      = begin
          let _acc = make(<deque>);
          for (goal in body)
            if (~ (dcg-normal-goal-p(goal) | dcg-word-list-p(goal)
                    | starts-with(goal, #"ex")
                    | not(instance?(goal, <list>))))
              push-last(_acc, last1(goal));
            end if;
          finally
            _acc;
          end for;
        end;
  select (length(sems))
    0
       => apply(list, #"=", ?sem, #(#"t"));
    1
       => list(#"=", ?sem, first(sems));
    otherwise
       => list(#"and*", sems, ?sem);
  end select;
end method collect-sems;

define method and*/2 (in, out, cont)
  // IN is a list of conjuncts that are conjoined into OUT.
  //  E.g.: (and* (t (and a b) t (and c d) t) ?x) ==>
  //         ?x = (and a b c d)
  if (unify!(out, maybe-add(#"and", conjuncts(pair(#"and", in)), #t)))
    cont();
  end if;
end method and*/2;

define method conjuncts (exp)
  // Get all the conjuncts from an expression.
  deref(exp);
  if (exp == #t)
    #f;
  elseif (not(instance?(exp, <list>)))
    list(exp);
  elseif (deref(first(exp)) == #())
    #f;
  elseif (first(exp) == #"and")
    mappend(conjuncts, tail(exp));
  else
    list(exp);
  end if;
end method conjuncts;

// LTD: No macros.
#"ex";

define variable *examples* = make(<table>, test: \==);

define method get-examples (category)
  *examples*[category];
end method get-examples;

define method clear-examples ()
  size(*examples*) := 0;
end method clear-examples;

define method add-examples (category, args, examples)
  // Add these example strings to this category,
  //   and when it comes time to run them, use the args.
  for (example in examples)
    if (instance?(example, <string>))
      let ex
          = list(example,
                 pair(category,
                      concatenate(args,
                                  pair(string->list(remove-punctuation(example)),
                                       #(#"()")))));
      if (~ member?(ex, get-examples(category), test: \=))
        *examples*[category]
                    := concatenate!(get-examples(category), list(ex));
      end if;
    end if;
  end for;
end method add-examples;

define method run-examples (#key category)
  // Run all the example phrases stored under a category.
  //   With no category, run ALL the examples.
  prolog-compile-symbols();
  if (empty?(category))
    do(method (cat, val)
         (method (s, #rest args)
            apply(maybe-initiate-xp-printing,
                  method (xp, #rest args)
                    begin
                      multiple-newlines1(xp, fresh: 2);
                      write-string++("Examples of ", xp, 0, 12);
                      fluid-bind (*print-escape* = #f)
                        write+(pop!(args), xp);
                      end fluid-bind;
                      write-char++(':', xp);
                      pprint-newline+(fresh: xp);
                    end;
                    if (args) copy-sequence(args); end if;
                  end method,
                  s, args);
          end method)(#t, cat);
         run-examples(cat);
       end method,
       key-sequence(*examples*), *examples*);
  else
    for (example in get-examples(category))
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   multiple-newlines1(xp, fresh: 2);
                   write-string++("EXAMPLE: ", xp, 0, 9);
                   let args = pop!(args);
                   block (return)
                     local method go-l ()
                             if (empty?(args)) return(#f); end if;
                             fluid-bind (*print-escape* = #f)
                               write+(pop!(args), xp);
                             end fluid-bind;
                             pprint-newline+(fresh: xp);
                             pprint-tab+(line: 9, 1, xp);
                             fluid-bind (*print-escape* = #f)
                               write+(pop!(args), xp);
                             end fluid-bind;
                             go-l();
                           end method go-l;
                     go-l();
                   end block;
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(#t, example);
      top-level-prove(tail(example));
    end for;
  end if;
end method run-examples;

define method remove-punctuation (string)
  // Replace punctuation with spaces in string.
  replace-elements(string, punctuation-p, always(' '));
end method remove-punctuation;

define method string->list (string)
  // Convert a string to a list of words.
  // LTD: Function READ-FROM-STRING not yet implemented.
  read-from-string(concatenate-as(<string>, "(", string, ")"));
end method string->list;

define method punctuation-p (char)
  cl-find(char, "*_.,;:`!?#-()\\\"");
end method punctuation-p;

// LTD: No macros.
#"conj-rule";

define method handle-conj (head)
  // Replace (Cat ...) with (Cat_ ...) if Cat is declared
  //   as a conjunctive category.
  if (instance?(head, <list>) & conj-category(predicate(head)))
    pair(conj-category(predicate(head)), args(head));
  else
    head;
  end if;
end method handle-conj;

define method conj-category (predicate)
  // If this is a conjunctive predicate, return the Cat_ symbol.
  symbol-get-property(predicate, #"conj-category");
end method conj-category;

