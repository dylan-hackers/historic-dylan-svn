//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File eliza1.lisp: Basic version of the Eliza program
define method variable-p (x)
  // Is x a variable (a symbol beginning with `?')?
  instance?(x, <symbol>) & as(<string>, x)[0] = '?';
end method variable-p;

//  ==============================
// Indicates pat-match failure
define constant fail = #f;

// Indicates pat-match success, with no variables.
define constant no-bindings = #(#(#"t" . #"t"));

//  ==============================
define method get-binding (var, bindings)
  // Find a (variable . value) pair in a binding list.
  cl-assoc(var, bindings);
end method get-binding;

define method binding-val (binding)
  // Get the value part of a single binding.
  tail(binding);
end method binding-val;

define method lookup (var, bindings)
  // Get the value part (for var) from a binding list.
  binding-val(get-binding(var, bindings));
end method lookup;

define method extend-bindings (var, val, bindings)
  // Add a (var . value) pair to a binding list.
  pair(pair(var, val), bindings);
end method extend-bindings;

//  ==============================
define method pat-match (pattern, input, #key bindings = no-bindings)
  // Match pattern against input in the context of the bindings
  if (bindings == fail)
    fail;
  elseif (variable-p(pattern))
    match-variable(pattern, input, bindings);
  elseif (pattern == input)
    bindings;
  elseif (instance?(pattern, <pair>) & instance?(input, <pair>))
    pat-match(tail(pattern), tail(input),
              pat-match(first(pattern), first(input), bindings));
  else
    fail;
  end if;
end method pat-match;

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

//  ==============================
define method extend-bindings (var, val, bindings)
  // Add a (var . value) pair to a binding list.
  pair(pair(var, val),
       //  Once we add a "real" binding,
       //  we can get rid of the dummy no-bindings
       if (bindings == no-bindings) #f; else bindings; end if);
end method extend-bindings;

//  ==============================
define method pat-match (pattern, input, #key bindings = no-bindings)
  // Match pattern against input in the context of the bindings
  if (bindings == fail)
    fail;
  elseif (variable-p(pattern))
    match-variable(pattern, input, bindings);
  elseif (pattern == input)
    bindings;
  elseif (segment-pattern-p(pattern))
    //  ***
    segment-match(pattern, input, bindings);
    //  ***
    elseif (instance?(pattern, <pair>) & instance?(input, <pair>))
    pat-match(tail(pattern), tail(input),
              pat-match(first(pattern), first(input), bindings));
  else
    fail;
  end if;
end method pat-match;

define method segment-pattern-p (pattern)
  // Is this a segment matching pattern: ((?* var) . pat)
  instance?(pattern, <pair>) & starts-with(first(pattern), #"?*");
end method segment-pattern-p;

//  ==============================
define method segment-match (pattern, input, bindings, #key start = 0)
  // Match the segment pattern ((?* var) . pat) against input.
  let var = second(first(pattern));
  let pat = tail(pattern);
  if (empty?(pat))
    match-variable(var, input, bindings);
  else
    let pos
        = find-key(copy-subsequence(input, start: start),
                   curry(\==, first(pat)));
    if (empty?(pos))
      fail;
    else
      let b2 = pat-match(pat, copy-sequence(input, pos), bindings);
      //  If this match failed, try another longer one
      //  If it worked, check that the variables match
      if (b2 == fail)
        segment-match(pattern, input, bindings, pos + 1);
      else
        match-variable(var, copy-sequence(input, 0, pos), b2);
      end if;
    end if;
  end if;
end method segment-match;

//  ==============================
define method segment-match (pattern, input, bindings, #key start = 0)
  // Match the segment pattern ((?* var) . pat) against input.
  let var = second(first(pattern));
  let pat = tail(pattern);
  if (empty?(pat))
    match-variable(var, input, bindings);
  else
    let pos
        = find-key(copy-subsequence(input, start: start),
                   curry(\==, first(pat)));
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

//  ==============================
define method rule-pattern (rule) first(rule); end method rule-pattern;

define method rule-responses (rule) tail(rule); end method rule-responses;

//  ==============================
define variable *eliza-rules* =
  #(#(#(#(#"?*", #"?x"), #"hello", #(#"?*", #"?y")),
      #(#"how", #"do", #"you", #"do.", #"please", #"state", #"your",
        #"problem.")),
    #(#(#(#"?*", #"?x"), #"i", #"want", #(#"?*", #"?y")),
      #(#"what", #"would", #"it", #"mean", #"if", #"you", #"got", #"?y"),
      #(#"why", #"do", #"you", #"want", #"?y"),
      #(#"suppose", #"you", #"got", #"?y", #"soon")),
    #(#(#(#"?*", #"?x"), #"if", #(#"?*", #"?y")),
      #(#"do", #"you", #"really", #"think", #"its", #"likely", #"that",
        #"?y"),
      #(#"do", #"you", #"wish", #"that", #"?y"),
      #(#"what", #"do", #"you", #"think", #"about", #"?y"),
      #(#"really--", #"if", #"?y")),
    #(#(#(#"?*", #"?x"), #"no", #(#"?*", #"?y")), #(#"why", #"not?"),
      #(#"you", #"are", #"being", #"a", #"bit", #"negative"),
      #(#"are", #"you", #"saying", "NO", #"just", #"to", #"be",
        #"negative?")),
    #(#(#(#"?*", #"?x"), #"i", #"was", #(#"?*", #"?y")),
      #(#"were", #"you", #"really?"),
      #(#"perhaps", #"i", #"already", #"knew", #"you", #"were", #"?y"),
      #(#"why", #"do", #"you", #"tell", #"me", #"you", #"were", #"?y",
        #"now?")),
    #(#(#(#"?*", #"?x"), #"i", #"feel", #(#"?*", #"?y")),
      #(#"do", #"you", #"often", #"feel", #"?y", #"?")),
    #(#(#(#"?*", #"?x"), #"i", #"felt", #(#"?*", #"?y")),
      #(#"what", #"other", #"feelings", #"do", #"you", #"have?")));

//  ==============================
define method eliza ()
  // Respond to user input using pattern matching rules.
  while (#t)
    print(#"eliza>", *standard-output*);
    print(flatten(use-eliza-rules(// LTD: Function READ not yet implemented.
                                  read())),
          *standard-output*, pretty: t);
  end while;
end method eliza;

define method use-eliza-rules (input)
  // Find some rule with which to transform the input.
  any?(method (rule)
         let result = pat-match(rule.rule-pattern, input);
         if (~ (result == fail))
           replace-multiple-in-tree(switch-viewpoint(result),
                                    random-elt(rule-responses(rule)));
         end if;
       end method,
       *eliza-rules*);
end method use-eliza-rules;

define method switch-viewpoint (words)
  // Change I to you and vice versa, and so on.
  replace-multiple-in-tree(#(#(#"i" . #"you"), #(#"you" . #"i"),
                             #(#"me" . #"you"), #(#"am" . #"are")),
                           words);
end method switch-viewpoint;

//  ==============================
define method flatten (the-list)
  // Append together elements (or lists) in the list.
  mappend(mklist, the-list);
end method flatten;

define method mklist (x)
  // Return x if it is a list, otherwise (x).
  if (instance?(x, <list>)) x; else list(x); end if;
end method mklist;

define method mappend (fn, the-list)
  // Apply fn to each element of list and append the results.
  apply(concatenate, map(fn, the-list));
end method mappend;

define method random-elt (choices)
  // Choose an element from a list at random.
  choices[random-uniform(to: size(choices))];
end method random-elt;

//  ==============================
// > (eliza)
// ELIZA> (hello there)
// (HOW DO YOU DO. PLEASE STATE YOUR PROBLEM.) 
// ELIZA> (i want to test this program)
// (WHAT WOULD IT MEAN IF YOU GOT TO TEST THIS PROGRAM) 
// ELIZA> (i could see if it works)
// (DO YOU REALLY THINK ITS LIKELY THAT IT WORKS)
// ELIZA> (no not really)
// (ARE YOU SAYING "NO" JUST TO BE NEGATIVE?) 
// ELIZA> (no)
// (ARE YOU SAYING "NO" JUST TO BE NEGATIVE?) 
// ELIZA> (forget it-- i was wondering how general the program is)
// (WHY DO YOU TELL ME YOU WERE WONDERING HOW GENERAL THE PROGRAM IS NOW?)
// ELIZA> (i felt like it)
// (WHAT OTHER FEELINGS DO YOU HAVE?) 
// ELIZA> (i feel this is enough)
// (DO YOU OFTEN FEEL THIS IS ENOUGH ?) 
// ELIZA> [Abort]
//  ==============================
"eof";

