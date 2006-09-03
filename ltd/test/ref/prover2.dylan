//  PROVER.CL
//  A verifier for propositions using Wang's algorithm.
//  (C) Copyright 1995 by Steven L. Tanimoto.
//  This program is described in Chapter 6 ("Logical Reasoning") of
//  "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
//  published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
//  Permission is granted for noncommercial use and modification of
//  this program, provided that this copyright notice is retained
//  and followed by a notice of any modifications made to the program.
//  PROVER is the interactive top-level shell.
define method prover ()
  // Top-level loop for a propositional validator using Wang's algorithm.
  let s = #f;
  block (return)
    while (#t)
      format-out("\nPlease enter proposition or HELP or RETURN.\n");
      s
       := // LTD: Function READ not yet implemented.
          read();
      if (s == #"help")
        format-out("Here's an example: ");
        format-out("((a and (not b)) implies a) \n");
      elseif (s == #"return")
        return(#f);
      elseif (s := block (syntax-error) reformat(s); end block)
        if (valid(#f, list(s)))
          format-out(" is valid. \n");
        else
          format-out(" is NOT valid. \n");
        end if;
      else
        format-out(": Syntax error \n");
      end if;
    end while;
  end block;
end method prover;

//  VALID is the main recursive workhorse that verifies
//  the propositional logic "theorems" with Wang's rules.
//  Pattern-matching results are passed back as and stored
//  locally as the value of B -- the "bindings".
//  Arguments L and R are the left and right sides of a sequent.
define method valid (l, r)
  // Returns T if the conjunction of the formulas in L implies
  //    any of the formulas in R.
  let b = #f;
  if (intersection(l, r))
    #t;
    //  NOT on the left:
    elseif (b
             := match(#(#(#"*", #"x"), #(#"not-wff", #"y"), #(#"*", #"z")),
                      l))
    valid(concatenate(val(#"x", b), val(#"z", b)),
          concatenate(r, tail(val(#"y", b))));
    //  NOT on the right:
    elseif (b
             := match(#(#(#"*", #"x"), #(#"not-wff", #"y"), #(#"*", #"z")),
                      r))
    valid(concatenate(l, tail(val(#"y", b))),
          concatenate(val(#"x", b), val(#"z", b)));
    //  OR on the right:
    elseif (b
             := match(#(#(#"*", #"x"), #(#"or-wff", #"y"), #(#"*", #"z")), r))
    valid(l,
          concatenate(val(#"x", b), list(first(val(#"y", b))),
                      tail(tail(val(#"y", b))), val(#"z", b)));
    //  AND on the left:
    elseif (b
             := match(#(#(#"*", #"x"), #(#"and-wff", #"y"), #(#"*", #"z")),
                      l))
    valid(concatenate(val(#"x", b), list(first(val(#"y", b))),
                      tail(tail(val(#"y", b))), val(#"z", b)),
          r);
    //  OR on the left:
    elseif (b
             := match(#(#(#"*", #"x"), #(#"or-wff", #"y"), #(#"*", #"z")), l))
    valid(concatenate(val(#"x", b), list(first(val(#"y", b))), val(#"z", b)),
          r)
     & valid(concatenate(val(#"x", b), tail(tail(val(#"y", b))),
                         val(#"z", b)),
             r);
    //  AND on the right:
    elseif (b
             := match(#(#(#"*", #"x"), #(#"and-wff", #"y"), #(#"*", #"z")),
                      r))
    valid(l,
          concatenate(val(#"x", b), list(first(val(#"y", b))), val(#"z", b)))
     & valid(l,
             concatenate(val(#"x", b), tail(tail(val(#"y", b))),
                         val(#"z", b)));
  end if;
end method valid;

define method or-wff (x)
  // Returns T if X if of the form (f1 OR f2).
  if (not(instance?(x, <list>))) #f; else second(x) == #"or"; end if;
end method or-wff;

define method and-wff (x)
  // Returns T if X is of the form (f1 AND f2).
  if (not(instance?(x, <list>))) #f; else second(x) == #"and"; end if;
end method and-wff;

define method not-wff (x)
  // Returns T if X is of the form (NOT f).
  if (not(instance?(x, <list>))) #f; else first(x) == #"not"; end if;
end method not-wff;

define method wff (x)
  // Returns T if X is a well-formed formula.
  if (not(instance?(x, <list>)))
    #t;
  elseif (match(#(#"not", #(#"wff", #"dum")), x))
    #t;
  elseif (match(#(#(#"wff", #"dum"), #(#"op", #"dum"), #(#"wff", #"dum")), x))
    #t;
  else
    #f;
  end if;
end method wff;

define method op (x)
  // Returns T if X is a recognized logical operator.
  member?(x, #(#"and", #"or", #"implies"));
end method op;

//  REFORMAT checks syntax and eliminates IMPLIES.
define method reformat (x)
  // Either returns the formula X with all occurrences
  //    of IMPLIES eliminated, or performs a THROW with the
  //    indication of a syntax-error.
  if (not(instance?(x, <list>)))
    x;
  elseif (empty?(wff(x)))
    syntax-error(#f);
  elseif (not-wff(x))
    list(#"not", reformat(second(x)));
  elseif (second(x) = #"implies")
    list(list(#"not", reformat(first(x))), #"or", reformat(third(x)));
  else
    list(reformat(first(x)), second(x), reformat(third(x)));
  end if;
end method reformat;

//  The following code is from MATCH2.CL
define method match (p, s)
  // Attempts to find a correspondence between P and S, utilizing
  //    any special constructs appearing in P.  Return an association
  //    list of bindings if successful; NIL otherwise.
  let _that = #f;
  if (_that := handle-both-null(p, s))
    _that;
  elseif (_that := handle-normal-recursion(p, s))
    _that;
  elseif (not(instance?(first(p), <list>)))
    #f;
  elseif (_that := handle-?(p, s))
    _that;
  elseif (_that := handle-*(p, s))
    _that;
  elseif (_that := handle-restrict-pred(p, s))
    _that;
  else
    #f;
  end if;
end method match;

define method handle-both-null (p, s)
  // Test for and handle case when both P and S are null.
  if (empty?(p) & empty?(s)) #(#(#"yes" . #"yes")); end if;
end method handle-both-null;

define method handle-normal-recursion (p, s)
  // Test for and handle case when the first elements of
  //     P and S are EQL.
  if (not(instance?(first(p), <list>)))
    if (first(p) == first(s)) match(tail(p), tail(s)); end if;
  end if;
end method handle-normal-recursion;

define method handle-? (p, s)
  // Test for and handle the case when (FIRST P) is of
  //    the form (? X).
  if (s)
    //  S must not be null
    if (first(first(p)) == #"?")
      let rest-match = match(tail(p), tail(s));
      if (rest-match)
        cons(cons(first(tail(first(p))), first(s)), rest-match);
      end if;
    end if;
  end if;
end method handle-?;

define method handle-* (p, s)
  // Test for and handle the case when (FIRST P) is of
  //    the form (* X).
  if (first(first(p)) == #"*")
    let pattern-variable = first(tail(first(p)));
    let rest-match = #f;
    if (s & (rest-match := match(tail(p), tail(s))))
      cons(cons(pattern-variable, list(first(s))), rest-match);
      //  subcase 2 --match no elements of S:
      elseif (rest-match := match(tail(p), s))
      cons(cons(pattern-variable, #f), rest-match);
      //  subcase 3 --match more than one element of S:
      elseif (s & (rest-match := match(p, tail(s))))
      cons(cons(pattern-variable,
                pair(first(s), val(pattern-variable, rest-match))),
           tail(rest-match));
    else
      #f;
    end if;
  end if;
end method handle-*;

define method handle-restrict-pred (p, s)
  // Handle case when (FIRST P) is of the form (PREDICATE X).
  if (s)
    //  S must not be null
    if (member?(first(first(p)), #(#"?", #"*")))
      //  Don't apply '? or '*.
      #f;
    elseif (apply(head(head(p)), list(first(s))))
      begin
        let rest-match = match(tail(p), tail(s));
        let pattern-variable = first(tail(first(p)));
        if (rest-match)
          cons(cons(pattern-variable, first(s)), rest-match);
        end if;
      end;
    end if;
  end if;
end method handle-restrict-pred;

//  The function VAL provides convenient access to
//  something matched by a variable after matching with MATCH.
define method val (variable, alist)
  // Returns the value associated with VARIABLE on ALIST.
  tail(cl-assoc(variable, alist));
end method val;

//  end of code from MATCH2.CL
//  Now invoke the program and provide test data...
prover();

// (a or (not a))
// ((a or (not a)) and (b or (not b)))
"eof";

