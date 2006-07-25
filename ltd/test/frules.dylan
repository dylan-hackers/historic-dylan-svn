//  -*- Mode: Lisp; -*-
//  This file is frules.lisp
//  Last edited: 7/12/93, by KDF
//  Copyright (c) 1988-1991, Kenneth D. Forbus, Northwestern University,
//  and Johan de Kleer, the Xerox Corporation.
//  All rights reserved.
//  See the file legal.txt for a paragraph stating scope of permission
//  and disclaimer of warranty.  The above copyright notice and that
//  paragraph must be included in any separate copy of this file.
"(in-package common-lisp-user)";

#f;

define class <rule> (<object>)
  slot rule-id, init-keyword: #"rule-id";
  // unique "name"
  slot rule-dbclass, init-keyword: #"rule-dbclass";
  // Dbclass it is linked to.
  slot rule-matcher, init-keyword: #"rule-matcher";
  // procedure that performs the match.
  slot rule-body, init-keyword: #"rule-body";
  // procedure that does the rule's work.
  slot rule-assumption?, init-keyword: #"rule-assumption?";
end class <rule>;

// Does it make an assumption?
define method ftre-rule-printer (r, st, ignore)
  format(st, "<Rule %D>", r.rule-id);
end method ftre-rule-printer;

define variable *file-counter* = 0;

define variable *file-prefix* = "";

// LTD: No macros.
#"rule-file";

//  Building and installing rules
// LTD: No macros.
#"rule";

// LTD: No macros.
#"a-rule";

//  Restriction: An a-rule can have only one trigger!
define method parse-triggers (trigger-list)
  if (empty?(trigger-list))
    #f;
  else
    let (var, test, new-triggers)
        = parse-trigger-options(tail(trigger-list), #f, #f);
    pair(list(head(trigger-list), var, test), parse-triggers(new-triggers));
  end if;
end method parse-triggers;

define method parse-trigger-options (triggers, var, test)
  select (car(triggers))
    #"var"
       => parse-trigger-options(tail(tail(triggers)), second(triggers), test);
    #"test"
       => parse-trigger-options(tail(tail(triggers)), var, second(triggers));
    otherwise
       => values(var, test, triggers);
  end select;
end method parse-trigger-options;

define method do-rule (triggers, body, asn?)
  fluid-bind (*rule-procedures* = #f)
    fluid-bind (*bound-vars* = #f)
      let index-form = #f;
      if (asn? & tail(triggers))
        error("\n a-rules can only have one trigger:\n%S,\n%S.", triggers,
              body);
      end if;
      index-form
       := build-rule(head(triggers),
                     replace-in-tree(#"internal-rule", #"rule",
                                     make-nested-rule(tail(triggers), body)),
                     asn?);
      //  Returning this ensures that all procedure definitions
      //  are executed before any indexing occurs.
      bq-cons(#"progn", bq-append(*rule-procedures*, bq-list(index-form)));
    end fluid-bind;
  end fluid-bind;
end method do-rule;

// LTD: No macros.
#"internal-rule";

define method make-nested-rule (triggers, body)
  if (empty?(triggers))
    body;
  else
    bq-list(bq-list(#"add-internal-rule", head(triggers),
                    make-nested-rule(tail(triggers), body)));
  end if;
end method make-nested-rule;

// LTD: No macros.
#"add-internal-rule";

//  Building a rule
define method build-rule (trigger, body, asn?)
  let (pattern, var, test) = parse-rule-trigger(trigger);
  match-procedure := generate-match-procedure(pattern, var, test);
  body-procedure := generate-body-procedure(pattern, var, body);
  push!(match-procedure, *rule-procedures*);
  push!(body-procedure, *rule-procedures*);
  bq-list(#"insert-rule",
          bq-list*(#"get-dbclass", get-trigger-dbclass(pattern),
                   #(#"*ftre*")),
          bq-list(#"function",
                  if (*bound-vars*)
                    bq-list(#"lambda", #(#"p"),
                            bq-list*(second(match-procedure), #"p",
                                     *bound-vars*));
                  else
                    second(match-procedure);
                  end if),
          bq-list(#"function",
                  if (*bound-vars*)
                    let tv = reverse!(pattern-free-variables(trigger));
                    bq-list(#"lambda", tv,
                            bq-cons(second(body-procedure),
                                    bq-append(tv,
                                              scratchout(tv, *bound-vars*))));
                  else
                    second(body-procedure);
                  end if),
          asn?);
end method build-rule;

define method parse-rule-trigger (trigger)
  //  A trigger has the form (<pattern> <options>)
  //   where <options> can be empty, or 
  //   :TEST <code> and/or :VAR <var>, where <code> must be
  //   non-nil for the match to succeed, and <var> will be
  //   bound to the whole pattern.
  if (variable?(trigger))
    trigger;
  elseif (instance?(trigger, <list>))
    apply(values, trigger);
  else
    error("Invalid expression in trigger: %S.", trigger);
  end if;
end method parse-rule-trigger;

define method get-trigger-dbclass (trigger)
  if (variable?(trigger))
    if (member?(trigger, *bound-vars*))
      trigger;
    else
      error("\nTrigger dbclass is unbound -- %S.", trigger);
    end if;
  elseif (not(instance?(trigger, <list>)))
    list(#"quote", trigger);
  else
    get-trigger-dbclass(head(trigger));
  end if;
end method get-trigger-dbclass;

//  Generating the body procedure
//  Macro for generate-body-procedure
//  (macros must be defined before use or compiler dies)
// LTD: No macros.
#"with-pushed-variable-bindings";

define method generate-body-procedure (pattern, var, body)
  newly-bound := pattern-free-variables(pattern);
  if (var) push!(var, newly-bound); end if;
  body := with-pushed-variable-bindings(newly-bound, fully-expand-body(body));
  env := concatenate(newly-bound, scratchout(newly-bound, *bound-vars*));
  bq-list*(#"defun", generate-rule-procedure-name(pattern), env, body);
end method generate-body-procedure;

define method generate-match-procedure (pattern, var, test)
  let (tests, binding-specs)
      = //  Construct a defun specialized to match the given pattern.
        //  That procedure will return NIL if no match,
        //    (values T <binding-spec>) if match is successful.
      generate-match-body(pattern, pattern-free-variables(pattern), test);
  bq-list(#"defun", generate-rule-procedure-name(pattern),
          bq-cons(#"p", *bound-vars*),
          bq-list(#"if", bq-cons(#"and", tests),
                  bq-list(#"values", #"t",
                          if (empty?(var) & empty?(binding-specs))
                            #f;
                          else
                            bq-cons(#"list",
                                    bq-append(if (var) #(#"p"); end if,
                                              reverse(binding-specs)));
                          end if)));
end method generate-match-procedure;

define method scratchout (l1, l2)
  // non-destructive and order-preserving
  for (el1 in l1) l2 := remove(l2, el1); finally l2; end for;
end method scratchout;

define method generate-rule-procedure-name (pattern)
  as(<symbol>,
     format(#f, "%S-%S-%S", *file-prefix*, pattern, inc!(*file-counter*)));
end method generate-rule-procedure-name;

//  Recursive macroexpansion
#f;

*macros-to-expand*
 := #(#"rule", #"a-rule", #"rlet", #"rassert!", #"internal-rule",
      #"add-internal-rule", #"with-pushed-variable-bindings");

define method fully-expand-body (body)
  if (empty?(body))
    #f;
  elseif (~ instance?(body, <list>))
    body;
  elseif (instance?(head(body), <symbol>))
    if (member?(head(body), *macros-to-expand*))
      fully-expand-body(// LTD: Function MACROEXPAND not yet implemented.
                        macroexpand(body));
    else
      pair(head(body), fully-expand-body(tail(body)));
    end if;
  else
    pair(fully-expand-body(head(body)), fully-expand-body(tail(body)));
  end if;
end method fully-expand-body;

//  Running rules
define method insert-rule (dbclass, matcher, body, asn?)
  with-ftre(dbclass-ftre(dbclass),
            rule
             := make-rule(matcher: matcher, body: body, dbclass: dbclass,
                          id: inc!(ftre-rule-counter(*ftre*)),
                          assumption?: asn?),
            //  Index it
            if (ftre-depth(*ftre*) = 0)
              push!(rule, dbclass-rules(dbclass));
            else
              push!(rule, ftre-local-rules(*ftre*));
            end if,
            for (candidate in get-candidates(dbclass-name(dbclass), *ftre*))
              try-rule-on(rule, candidate);
            end for);
end method insert-rule;

define method try-rules (fact, ftre)
  for (rule in get-candidate-rules(fact, ftre))
    try-rule-on(rule, fact);
  end for;
end method try-rules;

define method get-candidate-rules (fact, ftre)
  concatenate(ftre-local-rules(ftre), dbclass-rules(get-dbclass(fact, ftre)));
end method get-candidate-rules;

define method try-rule-on (rule, fact)
  with-ftre(dbclass-ftre(rule.rule-dbclass),
            let (okay?, bindings) = (rule.rule-matcher)(fact);
              if (okay?)
                enqueue(*ftre*, pair(rule.rule-body, bindings),
                        rule.rule-assumption?);
              end if);
end method try-rule-on;

define method run-rules (*ftre*)
  for (form = dequeue(*ftre*) then dequeue(*ftre*),
       counter = 0 then 1+(counter), until empty?(form))
    inc!(ftre-rules-run(*ftre*));
    with-ftre(*ftre*, //  Compare this to regular TRE!
              apply(head(form), tail(form)));
  finally
    debugging-ftre("~%  ~A(~A): ~A rules run.", *ftre*, ftre-depth(*ftre*),
                   counter);
  end for;
end method run-rules;

define method enqueue (ftre, new, asn?)
  if (asn?)
    push!(new, ftre-asn-queue(ftre));
  else
    push!(new, ftre-normal-queue(ftre));
  end if;
end method enqueue;

define method dequeue (ftre)
  if (ftre-normal-queue(ftre))
    pop!(ftre-normal-queue(ftre));
  else
    pop!(ftre-asn-queue(ftre));
  end if;
end method dequeue;

//  Displaying rules
define method show-rules (#key stream = *standard-output*)
  counter := 0;
  format(stream, "\n In global context:");
  let tab15015 = ftre-dbclass-table(*ftre*);
  do(method (key, dbclass)
       for (rule in dbclass-rules(dbclass))
         inc!(counter);
         print-rule(rule, stream);
       end for;
     end method,
     key-sequence(tab15015), tab15015);
  format(stream, "\n  %D global rules.", counter);
  if (ftre-depth(*ftre*) > 0)
    format(stream, "\n In current context:");
    for (rule in reverse(ftre-local-rules(*ftre*)))
      if (~ instance?(rule, <number>)) inc!(counter); end if;
      print-rule(rule, stream);
    end for;
  end if;
  counter;
end method show-rules;

define method print-rule (rule, #key stream = *standard-output*)
  format(stream, "\n %S(%S): %S, %S", rule,
         if (rule.rule-assumption?) "Y"; else "N"; end if, rule.rule-matcher,
         rule.rule-body);
end method print-rule;

define method get-rule (id, #key ftre = *ftre*)
  block (return-from-get-rule)
    let tab15015 = ftre-dbclass-table(ftre);
    do(method (key, dbclass)
         for (rule in dbclass-rules(dbclass))
           if (rule.rule-id = id) return-from-get-rule(rule); end if;
         end for;
       end method,
       key-sequence(tab15015), tab15015);
  end block;
end method get-rule;

