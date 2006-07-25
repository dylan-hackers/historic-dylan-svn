//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File eliza-pm.lisp: Updated version of eliza in section 6.3
requires("patmatch", "eliza");

define method eliza ()
  // Respond to user input using pattern matching rules.
  while (#t)
    print(#"eliza>", *standard-output*);
    print(flatten(use-eliza-rules(// LTD: Function READ not yet implemented.
                                  read())),
          *standard-output*);
  end while;
end method eliza;

define method use-eliza-rules (input)
  // Find some rule with which to transform the input.
  rule-based-translator(input, *eliza-rules*,
                        action: method (bindings, responses)
                                  replace-multiple-in-tree(switch-viewpoint(bindings),
                                                           random-elt(responses));
                                end method);
end method use-eliza-rules;

