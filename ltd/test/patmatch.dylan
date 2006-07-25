//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File pat-match.lisp: Pattern matcher from section 6.2
//  Two bug fixes By Richard Fateman, rjf@cs.berkeley.edu  October 92.
define method pat-match (pattern, input, #key bindings = no-bindings)
  // Match pattern against input in the context of the bindings
  if (bindings == fail)
    fail;
  elseif (variable-p(pattern))
    match-variable(pattern, input, bindings);
  elseif (pattern == input)
    bindings;
  elseif (segment-pattern-p(pattern))
    segment-matcher(pattern, input, bindings);
  elseif (single-pattern-p(pattern))
    //  ***
    single-matcher(pattern, input, bindings);
    //  ***
    elseif (instance?(pattern, <pair>) & instance?(input, <pair>))
    pat-match(tail(pattern), tail(input),
              pat-match(first(pattern), first(input), bindings));
  else
    fail;
  end if;
end method pat-match;

// Indicates pat-match failure
define constant fail = #f;

// Indicates pat-match success, with no variables.
define constant no-bindings = #(#(#"t" . #"t"));

define method variable-p (x)
  // Is x a variable (a symbol beginning with `?')?
  instance?(x, <symbol>) & as(<string>, x)[0] = '?';
end method variable-p;

define method get-binding (var, bindings)
  // Find a (variable . value) pair in a binding list.
  cl-assoc(var, bindings);
end method get-binding;

define method binding-var (binding)
  // Get the variable part of a single binding.
  head(binding);
end method binding-var;

define method binding-val (binding)
  // Get the value part of a single binding.
  tail(binding);
end method binding-val;

define method make-binding (var, val) pair(var, val); end method make-binding;

define method lookup (var, bindings)
  // Get the value part (for var) from a binding list.
  binding-val(get-binding(var, bindings));
end method lookup;

define method extend-bindings (var, val, bindings)
  // Add a (var . value) pair to a binding list.
  pair(make-binding(var, val),
       //  Once we add a "real" binding,
       //  we can get rid of the dummy no-bindings
       if (bindings == no-bindings) #f; else bindings; end if);
end method extend-bindings;

define method match-variable (var, input, bindings)
  // Does VAR match input?  Uses (or updates) and returns bindings.
  let binding = get-binding(var, bindings);
  if (~ binding)
    extend-bindings(var, input, bindings);
  elseif (input = binding-val(binding))
    bindings;
  else
    fail;
  end if;
end method match-variable;

symbol-get-property(#"?is", #"single-match") := #"match-is";

symbol-get-property(#"?or", #"single-match") := #"match-or";

symbol-get-property(#"?and", #"single-match") := #"match-and";

symbol-get-property(#"?not", #"single-match") := #"match-not";

symbol-get-property(#"?*", #"segment-match") := #"segment-match";

symbol-get-property(#"?+", #"segment-match") := #"segment-match+";

symbol-get-property(#"??", #"segment-match") := #"segment-match?";

symbol-get-property(#"?if", #"segment-match") := #"match-if";

define method segment-pattern-p (pattern)
  // Is this a segment-matching pattern like ((?* var) . pat)?
  instance?(pattern, <pair>) & instance?(first(pattern), <pair>)
   & instance?(first(first(pattern)), <symbol>)
   & segment-match-fn(first(first(pattern)));
end method segment-pattern-p;

define method single-pattern-p (pattern)
  // Is this a single-matching pattern?
  //   E.g. (?is x predicate) (?and . patterns) (?or . patterns).
  instance?(pattern, <pair>) & single-match-fn(first(pattern));
end method single-pattern-p;

define method segment-matcher (pattern, input, bindings)
  // Call the right function for this kind of segment pattern.
  (segment-match-fn(first(first(pattern))))(pattern, input, bindings);
end method segment-matcher;

define method single-matcher (pattern, input, bindings)
  // Call the right function for this kind of single pattern.
  (single-match-fn(first(pattern)))(tail(pattern), input, bindings);
end method single-matcher;

define method segment-match-fn (x)
  // Get the segment-match function for x, 
  //   if it is a symbol that has one.
  if (instance?(x, <symbol>))
    symbol-get-property(x, #"segment-match");
  end if;
end method segment-match-fn;

define method single-match-fn (x)
  // Get the single-match function for x, 
  //   if it is a symbol that has one.
  if (instance?(x, <symbol>)) symbol-get-property(x, #"single-match"); end if;
end method single-match-fn;

define method match-is (var-and-pred, input, bindings)
  // Succeed and bind var if the input satisfies pred,
  //   where var-and-pred is the list (var pred).
  let var = first(var-and-pred);
  let pred = second(var-and-pred);
  let new-bindings = pat-match(var, input, bindings);
  if (new-bindings == fail | ~ pred(input)) fail; else new-bindings; end if;
end method match-is;

define method match-and (patterns, input, bindings)
  // Succeed if all the patterns match the input.
  if (bindings == fail)
    fail;
  elseif (empty?(patterns))
    bindings;
  else
    match-and(tail(patterns), input,
              pat-match(first(patterns), input, bindings));
  end if;
end method match-and;

define method match-or (patterns, input, bindings)
  // Succeed if any one of the patterns match the input.
  if (empty?(patterns))
    fail;
  else
    let new-bindings = pat-match(first(patterns), input, bindings);
    if (new-bindings == fail)
      match-or(tail(patterns), input, bindings);
    else
      new-bindings;
    end if;
  end if;
end method match-or;

define method match-not (patterns, input, bindings)
  // Succeed if none of the patterns match the input.
  //   This will never bind any variables.
  if (match-or(patterns, input, bindings)) fail; else bindings; end if;
end method match-not;

define method segment-match (pattern, input, bindings, #key start = 0)
  // Match the segment pattern ((?* var) . pat) against input.
  let var = second(first(pattern));
  let pat = tail(pattern);
  if (empty?(pat))
    match-variable(var, input, bindings);
  else
    let pos = first-match-pos(first(pat), input, start);
    if (empty?(pos))
      fail;
    else
      let b2
          = pat-match(pat, copy-sequence(input, pos),
                      match-variable(var, copy-sequence(input, 0, pos),
                                     bindings));
      //  If this match failed, try another longer one
      if (b2 == fail)
        segment-match(pattern, input, bindings, pos + 1);
      else
        b2;
      end if;
    end if;
  end if;
end method segment-match;

define method first-match-pos (pat1, input, start)
  // Find the first position that pat1 could possibly match input,
  //   starting at position start.  If pat1 is non-constant, then just
  //   return start.
  if (not(instance?(pat1, <list>)) & ~ variable-p(pat1))
    find-key(copy-subsequence(input, start: start), curry(\==, pat1));
  elseif (start <= size(input))
    start;
    // *** fix, rjf 10/1/92 (was <)
    else
    #f;
  end if;
end method first-match-pos;

define method segment-match+ (pattern, input, bindings)
  // Match one or more elements of input.
  segment-match(pattern, input, bindings, 1);
end method segment-match+;

define method segment-match? (pattern, input, bindings)
  // Match zero or one element of input.
  let var = second(first(pattern));
  let pat = tail(pattern);
  pat-match(pair(var, pat), input, bindings)
   | pat-match(pat, input, bindings);
end method segment-match?;

define method match-if (pattern, input, bindings)
  // Test an arbitrary expression involving variables.
  //   The pattern looks like ((?if code) . rest).
  //  *** fix, rjf 10/1/92 (used to eval binding values)
  // LTD: Function PROGV not yet implemented.
  progv(map(head, bindings), map(tail, bindings),
        // LTD: Function EVAL not yet implemented.
        eval(second(first(pattern))))
   & pat-match(tail(pattern), input, bindings);
end method match-if;

define method pat-match-abbrev (symbol, expansion)
  // Define symbol as a macro standing for a pat-match pattern.
  symbol-get-property(symbol, #"expand-pat-match-abbrev")
   := expand-pat-match-abbrev(expansion);
end method pat-match-abbrev;

define method expand-pat-match-abbrev (pat)
  // Expand out all pattern matching abbreviations in pat.
  let _that = #f;
  if (_that
       := instance?(pat, <symbol>)
           & symbol-get-property(pat, #"expand-pat-match-abbrev"))
    _that;
  elseif (not(instance?(pat, <list>)))
    pat;
  else
    pair(expand-pat-match-abbrev(first(pat)),
         expand-pat-match-abbrev(tail(pat)));
  end if;
end method expand-pat-match-abbrev;

define method rule-based-translator (input, rules, #key matcher = pat-match,
                                     rule-if = first, rule-then = tail,
                                     action = replace-multiple-in-tree)
  // Find the first rule in rules that matches input,
  //   and apply the action to that rule.
  any?(method (rule)
         let result = matcher(rule-if(rule), input);
         if (~ (result == fail)) action(result, rule-then(rule)); end if;
       end method,
       rules);
end method rule-based-translator;

