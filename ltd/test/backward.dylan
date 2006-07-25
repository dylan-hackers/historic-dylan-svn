//  -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
//  Copyright 1992 Patrick H. Winston.  All rights reserved.
//  Version 1.1.1, copied from master file on 23 Apr 93       
//  
//  This software is licensed by Patrick H. Winston (licensor) for
//  instructional use with the textbooks ``Artificial Intelligence,'' by
//  Patrick H. Winston, and ``Lisp,'' by Patrick H. Winston and Berthold
//  K. P. Horn.  Your are free to make copies of this software and
//  modify it for such instructional use as long as:
//  1. You keep this notice intact.
//  2. You cause any modified files to carry a prominent notice stating
//     that you modified the files and the date of your modifications.
//  This software is licensed ``AS IS'' without warranty and the licensor
//  shall have no liability for any alleged defect or damages.
//  SPECIAL VARIABLES
define variable *assertions* = #f;

define variable *rules* = #f;

//  BACKWARD CHAINING
define method chain (#rest patterns)
  // 
  //   Purpose:	Initiate backward chaining.
  //
  let binding-stream
      = apply-filters(patterns, stream-cons(#f, #"empty-stream"));
  let variables = list-variables(patterns);
  let displayed-answers = #f;
  if (not(pair?(variables)))
    if (stream-endp(binding-stream)) #"no"; else #"yes"; end if;
  else
    block (return)
      for (binding-stream = binding-stream then stream-rest(binding-stream),
           until stream-endp(binding-stream))
        let answer = make-answer(variables, stream-first(binding-stream));
        if (~ member?(answer, displayed-answers, test: \=))
          display-answer(answer);
          displayed-answers := pair(answer, displayed-answers);
          // Only change needed to produce PROLOGlike version:
          if (',' = read-element(*standard-input*, nil))
            format-out(",");
          else
            return(#"no-more");
          end if;
        end if;
      finally
        #"no-more";
      end for;
    end block;
  end if;
end method chain;

//  AUXILIARIES BORROWED FROM FORWARD CHAINING
define method apply-filters (patterns, initial-input-stream)
  // 
  //   Purpose:	Tries to match all patterns to all assertions using
  // 		all binding lists.
  //
  if (not(pair?(patterns)))
    initial-input-stream;
  else
    apply-filters(tail(patterns),
                  filter-binding-stream(first(patterns),
                                        initial-input-stream));
  end if;
end method apply-filters;

define method match-pattern-to-assertions (pattern, bindings)
  // 
  //   Purpose:	Tries to match one pattern to all assertions using
  // 		one binding list.
  //
  stream-concatenate(stream-transform(method (assertion)
                                        try-assertion(pattern,
                                                      assertion,
                                                      bindings);
                                      end method,
                                      *assertions*));
end method match-pattern-to-assertions;

define method try-assertion (pattern, assertion, bindings)
  // 
  //   Purpose:	Tries to match one pattern to one assertion.
  //
  let result = match(pattern, assertion, bindings);
  if (#"fail" == result)
    #"empty-stream";
  else
    stream-cons(result, #"empty-stream");
  end if;
end method try-assertion;

//  AUXILIARIES BORROWED, WITH MODIFICATIONS, FROM FORWARD CHAINING
define method filter-binding-stream (pattern, stream)
  // 
  //   Purpose:	Tries to match one pattern to all assertions,
  // 		and all rule consequents, using
  // 		all binding lists.
  //   Remarks:	Tries rules as well as assertions.
  //
  stream-concatenate(stream-transform(method (bindings)
                                        stream-concatenate(stream-cons(match-pattern-to-assertions(pattern,
                                                                                                   bindings),
                                                                       stream-cons(match-pattern-to-rules(pattern,
                                                                                                          bindings),
                                                                                   #"empty-stream")));
                                      end method,
                                      stream));
end method filter-binding-stream;

define method instantiate-variables (pattern, a-list)
  // 
  //   Purpose:	Replaces variables by their bindings.
  //   Remarks:	More complicated than forward chaining version
  // 		because variables may be matched to variables.
  //
  if (not(instance?(pattern, <list>)))
    pattern;
  elseif (#"?" == first(pattern))
    begin
      let binding = find-binding(pattern, a-list);
      if (binding)
        instantiate-variables(extract-value(binding), a-list);
      else
        pattern;
      end if;
    end;
  else
    pair(instantiate-variables(first(pattern), a-list),
         instantiate-variables(tail(pattern), a-list));
  end if;
end method instantiate-variables;

//  RULE HANDLING AUXILIARIES
define method match-pattern-to-rules (pattern, bindings)
  // 
  //   Purpose:	Tries to match one pattern to all rules using
  // 		one binding list.
  //
  stream-concatenate(stream-transform(method (rule)
                                        try-rule(pattern, rule, bindings);
                                      end method,
                                      *rules*));
end method match-pattern-to-rules;

define method try-rule (pattern, rule, bindings)
  // 
  //   Purpose:	Tries to match one pattern to one rule.
  //
  let rule = make-variables-unique(rule);
  let result = unify(pattern, rule-then(rule), bindings);
  if (~ (#"fail" == result))
    format-out("\nTrying to establish");
    for (l in instantiate-variables(rule-then(rule), result))
      format-out(" %S", l);
    end for;
    format-out(" using rule %S.", rule-name(rule));
  end if;
  if (#"fail" == result)
    #"empty-stream";
  else
    let new-stream
        = apply-filters(rule-ifs(rule), stream-cons(result, #"empty-stream"));
    if (stream-endp(new-stream))
      format-out("\nRule %S fails to establish", rule-name(rule));
    else
      format-out("\nRule %S establishes", rule-name(rule));
    end if;
    for (l in instantiate-variables(rule-then(rule), result))
      format-out(" %S", l);
    end for;
    format-out(".");
    new-stream;
  end if;
end method try-rule;

define method hypothesize (assertion)
  // 
  //   Purpose:	Backward chains from one hypothesis.
  //
  select (chain(assertion))
    #"yes"
       => format-out("\nEvidently");
           for (l in assertion) format-out(" %S", l); end for;
           format-out(" is true.");
           #t;
    #"no"
       => format-out("\nEvidently");
           for (l in assertion) format-out(" %S", l); end for;
           format-out(" is false.");
           #f;
    otherwise
       => values();
  end select;
end method hypothesize;

//  MISCELLANEOUS AUXILIARY PROCEDURES
define method make-variables-unique (rule)
  // 
  //   Purpose:	Avoids conflict resulting from having the same variable
  // 		names appear in more than one rule.
  //
  let variables = list-variables(rule);
  let unique-variables
      = // Make substitution list:
      map(method (variable)
            let input
                = // LTD: Function MAKE-STRING-INPUT-STREAM not yet implemented.
                  make-string-input-stream(format(#f,
                                                  "%c-%S",
                                                  format(#f,
                                                         "%S",
                                                         variable)[0],
                                                  inc!(*name-counter*)),
                                           0,
                                           #f);
            block (nil)
              begin
                // LTD: Function READ not yet implemented.
                read(input);
              end;
            cleanup
              deallocate-resource(#"string-input-simple-stream", input);
            end block;
          end method,
          variables);
  instantiate-variables(rule, // Make binding list:
                        map(method (v, l) list(v, list(#"?", l)); end method,
                            variables, unique-variables));
end method make-variables-unique;

define method make-variables-unique (rule)
  // 
  //   Purpose:	Avoids conflict resulting from having the same variable
  // 		names appear in more than one rule.
  //
  let variables = list-variables(rule);
  let unique-variables
      = map(method (variable)
              generate-symbol(#"string"(format(#f, "%S", variable)));
            end method,
            variables);
  instantiate-variables(rule,
                        map(method (v, l) list(v, list(#"?", l)); end method,
                            variables, unique-variables));
end method make-variables-unique;

define method list-variables (pattern, #key names)
  // 
  //   Purpose:	Creates a list of variables appearing in a pattern.
  //
  if (not(instance?(pattern, <list>)))
    names;
  elseif (#"?" == first(pattern))
    // Ignore anonymous variable:
    if (#"_" == second(pattern) | member?(second(pattern), names))
      names;
    else
      concatenate(names, tail(pattern));
    end if;
  else
    list-variables(tail(pattern), list-variables(first(pattern), names));
  end if;
end method list-variables;

//  ANSWER PREPARATION
define method display-answer (answers)
  format-out("\n-->");
  for (answer in answers)
    format-out(" %S = %S", first(answer), second(answer));
  end for;
end method display-answer;

define method make-answer (variables, bindings)
  instantiate-variables(map(method (variable)
                              list(variable, list(#"?", variable));
                            end method,
                            variables),
                        bindings);
end method make-answer;

//  ASSERTION AND RULE ACCESS FUNCTIONS
define method remember-assertion (assertion)
  stream-remember(assertion, *assertions*);
end method remember-assertion;

define method remember-rule (rule)
  stream-remember(rule, *rules*);
end method remember-rule;

define method clear-assertions ()
  *assertions* := make-empty-stream();
end method clear-assertions;

define method clear-rules ()
  *rules* := make-empty-stream();
end method clear-rules;

define method display-assertions (#key stream = *assertions*)
  if (~ stream-endp(stream))
    print(stream-first(stream), *standard-output*);
    display-assertions(stream-rest(stream));
  end if;
end method display-assertions;

//  ACCESS FUNCTIONS FOR RULE ELEMENTS
define method rule-name (rule) first(rule); end method rule-name;

define method rule-ifs (rule)
  extract-from-rule(#"if", rule);
end method rule-ifs;

define method rule-thens (rule)
  extract-from-rule(#"then", rule);
end method rule-thens;

define method rule-then (rule) first(rule-thens(rule)); end method rule-then;

define method extract-from-rule (marker, rule)
  up-to-atom(tail(member?(marker, rule)));
end method extract-from-rule;

define method up-to-atom (rule)
  if (not(instance?(first(rule), <list>)))
    #f;
  else
    pair(first(rule), up-to-atom(tail(rule)));
  end if;
end method up-to-atom;

