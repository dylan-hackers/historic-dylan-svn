//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File mycin.lisp: Chapter 16's implementation of MYCIN.
//  A sample rulebase is provided in "mycin-rules.lisp".
define constant true = 1.0;

define constant false = -1.0;

define constant unknown = 0.0;

define method cf-or (a, b)
  // Combine the certainty factors for the formula (A or B).
  //   This is used when two rules support the same conclusion.
  if (a > 0 & b > 0)
    a + b + -1 * a * b;
  elseif (a < 0 & b < 0)
    a + b + a * b;
  else
    (a + b) / (1 - min(abs(a), abs(b)));
  end if;
end method cf-or;

define method cf-and (a, b)
  // Combine the certainty factors for the formula (A and B).
  min(a, b);
end method cf-and;

// Below this certainty we cut off search.
define constant cf-cut-off = 0.2;

define method true-p (cf)
  // Is this certainty factor considered true?
  cf-p(cf) & cf > cf-cut-off;
end method true-p;

define method false-p (cf)
  // Is this certainty factor considered false?
  cf-p(cf) & cf < cf-cut-off - 1.0;
end method false-p;

define method cf-p (x)
  // Is X a valid numeric certainty factor?
  instance?(x, <number>) & (false <= x & x <= true);
end method cf-p;

begin
  let db = make(<table>, test: \=);
  define method get-db (key) db[key]; end method get-db;
  define method put-db (key, val) db[key] := val; end method put-db;
  define method clear-db () size(db) := 0; end method clear-db;
end;

define method get-vals (parm, inst)
  // Return a list of (val cf) pairs for this (parm inst).
  get-db(list(parm, inst));
end method get-vals;

define method get-cf (parm, inst, val)
  // Look up the certainty factor or return unknown.
  second(cl-assoc(val, get-vals(parm, inst))) | unknown;
end method get-cf;

define method update-cf (parm, inst, val, cf)
  // Change the certianty factor for (parm inst is val),
  //   by combining the given cf with the old.
  let new-cf = cf-or(cf, get-cf(parm, inst, val));
  put-db(list(parm, inst),
         pair(list(val, new-cf),
              remove(get-db(list(parm, inst)), val,
                     test: method (x, y) x == first(y); end method)));
end method update-cf;

define constant help-string =
  "~&Type one of the following:\n ?     - to see possible answers for this parameter\n rule  - to show the current rule\n why   - to see why this question is asked\n help  - to see this list\n xxx   - (for some specific xxx) if there is a definite answer\n (xxx .5 yyy .4) - If there are several answers with \n                   different certainty factors.";

define method ask-vals (parm, inst)
  // Ask the user for the value(s) of inst's parm parameter,
  //   unless this has already been asked.  Keep asking until the
  //   user types UNKNOWN (return nil) or a valid reply (return t).
  if (~ get-db(list(#"asked", parm, inst)))
    put-db(list(#"asked", parm, inst), #t);
    block (return)
      while (#t)
        let ans = prompt-and-read-vals(parm, inst);
        select (ans)
          #"help"
             => help-string(#t);
          #"why"
             => print-why(get-db(#"current-rule"), parm);
          #"rule"
             => print(get-db(#"current-rule"), *standard-output*);
          (#"unk", #"unknown")
             => return(#f);
          #"?"
             => format-out("\nA %S must be of type %S", parm,
                           parm-type(parm));
                 #f;
          otherwise
             => if (check-reply(ans, parm, inst))
                  return(#t);
                else
                  (method (s, #rest args)
                     apply(maybe-initiate-xp-printing,
                           method (xp, #rest args)
                             begin
                               pprint-newline+(fresh: xp);
                               write-string++("Illegal reply.  ", xp, 0, 16);
                               write-string++("Type ? to see legal ones.",
                                              xp,
                                              0,
                                              25);
                             end;
                             if (args) copy-sequence(args); end if;
                           end method,
                           s, args);
                   end method)(#t);
                end if;
        end select;
      end while;
    end block;
  end if;
end method ask-vals;

define method prompt-and-read-vals (parm, inst)
  // Print the prompt for this parameter (or make one up) and
  //   read the reply.
  write-element(*standard-output*, '\n');
  (parm-prompt(get-parm(parm)))(#t, inst-name(inst), parm);
  print(" ", *standard-output*);
  force-output(*standard-output*);
  (parm-reader(get-parm(parm)))();
end method prompt-and-read-vals;

define method inst-name (inst)
  // The name of this instance.
  //  The stored name is either like (("Jan Doe" 1.0)) or nil
  first(first(get-vals(#"name", inst))) | inst;
end method inst-name;

define method check-reply (reply, parm, inst)
  // If reply is valid for this parm, update the DB.
  //   Reply should be a val or (val1 cf1 val2 cf2 ...).
  //   Each val must be of the right type for this parm.
  let answers = parse-reply(reply);
  if (every?(method (pair)
               instance?(first(pair), parm-type(parm)) & cf-p(second(pair));
             end method,
             answers))
    //  Add replies to the data base
    for (pair in answers)
      update-cf(parm, inst, first(pair), second(pair));
    end for;
    answers;
  end if;
end method check-reply;

define method parse-reply (reply)
  // Convert the reply into a list of (value cf) pairs.
  if (empty?(reply))
    #f;
  elseif (not(instance?(reply, <list>)))
    list(list(reply, true));
  else
    pair(list(first(reply), second(reply)), parse-reply(rest2(reply)));
  end if;
end method parse-reply;

define class <parm> (<object>)
  slot parm-name, init-keyword: #"parm-name";
  slot parm-context = #f, init-keyword: #"parm-context";
  slot parm-prompt = "~&What is the ~*~a of ~2:*~a?",
       init-keyword: #"parm-prompt";
  slot parm-ask-first = #f, init-keyword: #"parm-ask-first";
  slot parm-type-restriction = #t, init-keyword: #"parm-type-restriction";
  slot parm-reader = #"read", init-keyword: #"parm-reader";
end class <parm>;

// LTD: No macros.
#"defparm";

define method parm-type (parm-name)
  // What type is expected for a value of this parameter?
  parm-type-restriction(get-parm(parm-name));
end method parm-type;

define method get-parm (parm-name)
  // Look up the parameter structure with this name.
  //  If there is none, make one
  symbol-get-property(parm-name, #"parm")
   | (symbol-get-property(parm-name, #"parm") := new-parm(parm-name));
end method get-parm;

// LTD: Can't handle complex deftypes.
#f;

// A context is a sub-domain, a type.
define class <context> (<object>)
  slot context-name, init-keyword: #"context-name";
  slot context-number = 0, init-keyword: #"context-number";
  slot context-initial-data, init-keyword: #"context-initial-data";
  slot context-goals, init-keyword: #"context-goals";
end class <context>;

// LTD: No macros.
#"defcontext";

define method new-instance (context)
  // Create a new instance of this context.
  let instance
      = format(#f, "%S-%d", context.context-name,
               inc!(context.context-number));
  format-out("\n------ %S ------\n", instance);
  put-db(context.context-name, instance);
  put-db(#"current-instance", instance);
end method new-instance;

define class <rule> (<object>)
  slot rule-number, init-keyword: #"rule-number";
  slot rule-premises, init-keyword: #"rule-premises";
  slot rule-conclusions, init-keyword: #"rule-conclusions";
  slot rule-cf, init-keyword: #"rule-cf";
end class <rule>;

begin
  let rules = make(<table>);
  define method put-rule (rule)
    // Put the rule in a table, indexed under each
    //     parm in the conclusion.
    for (concl in rule.rule-conclusions)
      push!(rule, rules[first(concl)]);
    end for;
    rule;
  end method put-rule;
  define method get-rules (parm)
    // A list of rules that help determine this parameter.
    rules[parm];
  end method get-rules;
  define method clear-rules () size(rules) := 0; end method clear-rules;
end;

define method find-out (parm, #key inst = get-db(#"current-instance"))
  // Find the value(s) of this parameter for this instance,
  //   unless the values are already known.
  //   Some parameters we ask first; others we use rules first.
  get-db(list(#"known", parm, inst))
   | put-db(list(#"known", parm, inst),
            if (parm-ask-first(get-parm(parm)))
              (ask-vals(parm, inst) | use-rules(parm));
            else
              (use-rules(parm) | ask-vals(parm, inst));
            end if);
end method find-out;

define method use-rules (parm)
  // Try every rule associated with this parameter.
  //   Return true if one of the rules returns true.
  any?(true-p, map(use-rule, get-rules(parm)));
end method use-rules;

define method use-rule (rule)
  // Apply a rule to the current situation.
  //  Keep track of the rule for the explanation system:
  put-db(#"current-rule", rule);
  //  If any premise is known false, give up.
  //  If every premise can be proved true,  then
  //  draw conclusions (weighted with the certainty factor).
  if (~ any?(reject-premise, rule.rule-premises))
    let cf = satisfy-premises(rule.rule-premises, true);
    if (true-p(cf))
      for (conclusion in rule.rule-conclusions)
        conclude(conclusion, cf * rule.rule-cf);
      end for;
      cf;
    end if;
  end if;
end method use-rule;

define method satisfy-premises (premises, cf-so-far)
  // A list of premises is satisfied if they are all true.
  //   A combined cf is returned.
  //  cf-so-far is an accumulator of certainty factors
  if (empty?(premises))
    cf-so-far;
  elseif (~ true-p(cf-so-far))
    false;
  else
    satisfy-premises(tail(premises),
                     cf-and(cf-so-far, eval-condition(first(premises))));
  end if;
end method satisfy-premises;

define method eval-condition (condition, #key find-out-p = #t)
  // See if this condition is true, optionally using FIND-OUT
  //   to determine unknown parameters.
  let (parm, inst, op, val) = parse-condition(condition);
  if (find-out-p) find-out(parm, inst); end if;
  let _acc = 0;
  for (pair in get-vals(parm, inst))
    if (op(first(pair), val)) inc!(_acc, second(pair)); end if;
  finally
    _acc;
  end for;
end method eval-condition;

define method reject-premise (premise)
  // A premise is rejected if it is known false, without
  //   needing to call find-out recursively.
  false-p(eval-condition(premise, #f));
end method reject-premise;

define method conclude (conclusion, cf)
  // Add a conclusion (with specified certainty factor) to DB.
  let (parm, inst, op, val) = parse-condition(conclusion);
  update-cf(parm, inst, val, cf);
end method conclude;

define method is (a, b) a = b; end method is;

define method parse-condition (condition)
  // A condition is of the form (parm inst op val).
  //   So for (age patient is 21), we would return 4 values:
  //   (age patient-1 is 21), where patient-1 is the current patient.
  values(first(condition), get-db(second(condition)), third(condition),
         condition[3]);
end method parse-condition;

define method emycin (contexts)
  // An Expert System Shell.  Accumulate data for instances of each
  //   context, and solve for goals.  Then report the findings.
  clear-db();
  get-context-data(contexts);
end method emycin;

define method get-context-data (contexts)
  // For each context, create an instance and try to find out
  //   required data.  Then go on to other contexts, depth first,
  //   and finally ask if there are other instances of this context.
  if (~ empty?(contexts))
    let context = first(contexts);
    let inst = new-instance(context);
    put-db(#"current-rule", #"initial");
    let list92543 = context.context-initial-data;
    begin do(find-out, list92543); list92543; end;
    put-db(#"current-rule", #"goal");
    let list92543 = context.context-goals;
    begin do(find-out, list92543); list92543; end;
    report-findings(context, inst);
    get-context-data(tail(contexts));
    if (// LTD: Function Y-OR-N-P not yet implemented.
        y-or-n-p("Is there another ~a?", context.context-name))
      get-context-data(contexts);
    end if;
  end if;
end method get-context-data;

// LTD: No macros.
#"defrule";

define method check-conditions (rule-num, conditions, kind)
  // Warn if any conditions are invalid.
  if (empty?(conditions))
    format-out("Rule ~a: Missing ~a", rule-num, kind);
  end if;
  for (condition in conditions)
    if (~ instance?(condition, <pair>))
      format-out("Rule ~a: Illegal ~a: ~a", rule-num, kind, condition);
    end if;
    let (parm, inst, op, val) = parse-condition(condition);
    if (kind == #"conclusion" & ~ (op == #"is"))
      format-out("Rule ~a: Illegal operator (~a) in conclusion: ~a", rule-num,
                 op, condition);
    end if;
    if (~ instance?(val, parm-type(parm)))
      format-out("Rule ~a: Illegal value (~a) in ~a: ~a", rule-num, val, kind,
                 condition);
    end if;
  end for;
end method check-conditions;

define method report-findings (context, inst)
  // Print findings on each goal for this instance.
  if (context.context-goals)
    format-out("\nFindings for %S:", inst-name(inst));
    for (goal in context.context-goals)
      let values = get-vals(goal, inst);
      //  If there are any values for this goal,
      //  print them sorted by certainty factor.
      if (values)
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   begin
                     pprint-newline+(fresh: xp);
                     write-char++(' ', xp);
                     fluid-bind (*print-escape* = #f)
                       write+(pop!(args), xp);
                     end fluid-bind;
                     write-char++(':', xp);
                     let args = pop!(args);
                     block (return)
                       local method go-l ()
                               if (empty?(args)) return(#f); end if;
                               let args = pop!(args);
                               block (return)
                                 local method go-l ()
                                       if (empty?(args)) return(#f); end if;
                                       write-char++(' ', xp);
                                       fluid-bind (*print-escape* = #f)
                                       write+(pop!(args), xp);
                                       end fluid-bind;
                                       write-string++(" (", xp, 0, 2);
                                       using-format(xp, "~,3f", pop!(args));
                                       write-string++(")  ", xp, 0, 3);
                                       go-l();
                                       end method go-l;
                                 go-l();
                               end block;
                               go-l();
                             end method go-l;
                       go-l();
                     end block;
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(#t, goal,
                     sort!(copy-sequence(values),
                           test: method (x, y)
                                   second(x) > second(y);
                                 end method));
      else
        format-out("\n %S: unknown", goal);
      end if;
    end for;
  end if;
end method report-findings;

define method print-rule (rule, #key stream = #t, depth)
  format(stream, "\nRule %S:\n  If", rule.rule-number);
  print-conditions(rule.rule-premises, stream);
  format(stream, "\n  Then %S (%S) that", cf->english(rule.rule-cf),
         rule.rule-cf);
  print-conditions(rule.rule-conclusions, stream);
end method print-rule;

define method print-conditions (conditions, #key stream = #t, num = 1)
  // Print a list of numbered conditions.
  for (condition in conditions)
    print-condition(condition, stream, num);
  end for;
end method print-conditions;

define method print-condition (condition, stream, number)
  // Print a single condition in pseudo-English.
  (method (s, #rest args)
     apply(maybe-initiate-xp-printing,
           method (xp, #rest args)
             begin
               pprint-newline+(fresh: xp);
               write-string++("    ", xp, 0, 4);
               using-format(xp, "~d", pop!(args));
               write-char++(')', xp);
               let args = pop!(args);
               block (return)
                 local method go-l ()
                         if (empty?(args)) return(#f); end if;
                         write-char++(' ', xp);
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
   end method)(stream, number,
               begin
                 let parm = first(condition);
                 let inst = second(condition);
                 let op = third(condition);
                 let val = condition[3];
                 select (val)
                   #"yes"
                      => list(#"the", inst, op, parm);
                   #"no"
                      => list(#"the", inst, op, #"not", parm);
                   otherwise
                      => list(#"the", parm, #"of", #"the", inst, op, val);
                 end select;
               end);
end method print-condition;

define method cf->english (cf)
  // Convert a certainy factor to an English phrase.
  if (cf = 1.0)
    "there is certain evidence";
  elseif (cf > 0.8)
    "there is strongly suggestive evidence";
  elseif (cf > 0.5)
    "there is suggestive evidence";
  elseif (cf > 0.0)
    "there is weakly suggestive evidence";
  elseif (cf = 0.0)
    "there is NO evidence either way";
  elseif (cf < 0.0)
    concatenate-as(<string>, cf->english(- cf), " AGAINST the conclusion");
  end if;
end method cf->english;

define method print-why (rule, parm)
  // Tell why this rule is being used.  Print what is known,
  //   what we are trying to find out, and what we can conclude.
  format-out("\n[Why is the value of %S being asked for?]", parm);
  if (member?(rule, #(#"initial", #"goal")))
    format-out("\n%S is one of the %S parameters.", parm, rule);
  else
    let (knowns, unknowns)
        = partition-if(method (premise)
                         true-p(eval-condition(premise, #f));
                       end method,
                       rule.rule-premises);
    if (knowns)
      format-out("\nIt is known that:");
      print-conditions(knowns);
      format-out("\nTherefore,");
    end if;
    let new-rule = copy-rule(rule);
    new-rule.rule-premises := unknowns;
    print(new-rule, *standard-output*);
  end if;
end method print-why;

define method mycin ()
  // Determine what organism is infecting a patient.
  emycin(list(defcontext(patient, name(sex, age), #()),
              defcontext(culture, site(days-old), #()),
              defcontext(organism, #(), identity())));
end method mycin;

