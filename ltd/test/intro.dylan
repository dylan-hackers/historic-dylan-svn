//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File intro.lisp: Miscellaneous functions from the introduction.
define method last-name (name)
  // Select the last name from a name represented as a list.
  first(copy-sequence(name, start: size(name) - 1));
end method last-name;

define method first-name (name)
  // Select the first name from a name represented as a list.
  first(name);
end method first-name;

names
 := #(#(#"john", #"q", #"public"), #(#"malcolm", #"x"),
      #(#"admiral", #"grace", #"murray", #"hopper"), #(#"spot"),
      #(#"aristotle"), #(#"a", #"a", #"milne"), #(#"z", #"z", #"top"),
      #(#"sir", #"larry", #"olivier"), #(#"miss", #"scarlet"));

//  ==============================
// A list of titles that can appear at the start of a name.
define variable *titles* =
  #(#"mr", #"mrs", #"miss", #"ms", #"sir", #"madam", #"dr", #"admiral",
    #"major", #"general");

//  ==============================
define method first-name (name)
  // Select the first name from a name represented as a list.
  if (member?(first(name), *titles*))
    first-name(tail(name));
  else
    first(name);
  end if;
end method first-name;

//  ==============================
//  ==============================
define method numbers-and-negations (input)
  // Given a list, return only the numbers and their negations.
  mappend(number-and-negation, input);
end method numbers-and-negations;

define method number-and-negation (x)
  // If x is a number, return a list of x and -x.
  if (instance?(x, <number>)) list(x, - x); else #f; end if;
end method number-and-negation;

//  ==============================
define method mappend (fn, the-list)
  // Apply fn to each element of list and append the results.
  if (empty?(the-list))
    #f;
  else
    concatenate(fn(first(the-list)), mappend(fn, tail(the-list)));
  end if;
end method mappend;

//  ==============================
//  ==============================
define method atomprint (exp, #key depth = 0)
  // Print each atom in exp, along with its depth of nesting.
  if (not(instance?(exp, <list>)))
    format-out("\nATOM: %S, DEPTH %d", exp, depth);
  else
    for (element in exp) atomprint(element, depth + 1); end for;
  end if;
end method atomprint;

//  ==============================
define method power (x, n)
  // Power raises x to the nth power.  N must be an integer >= 0.
  //    This executes in log n time, because of the check for even n.
  if (n = 0)
    1;
  elseif (even?(n))
    power(x, (n / 2)) ^ 2;
  else
    x * power(x, (n - 1));
  end if;
end method power;

//  ==============================
define method count-atoms (exp)
  // Return the total number of non-nil atoms in the expression.
  if (empty?(exp))
    0;
  elseif (not(instance?(exp, <list>)))
    1;
  else
    count-atoms(first(exp)) + count-atoms(tail(exp));
  end if;
end method count-atoms;

define method count-all-atoms (exp, #key if-null = 1)
  // Return the total number of atoms in the expression, 
  //   counting nil as an atom only in non-tail position.
  if (empty?(exp))
    if-null;
  elseif (not(instance?(exp, <list>)))
    1;
  else
    count-all-atoms(first(exp), 1) + count-all-atoms(tail(exp), 0);
  end if;
end method count-all-atoms;

//  ==============================
define method count-anywhere (item, tree)
  // Count the times item appears anywhere within tree.
  if (item == tree)
    1;
  elseif (not(instance?(tree, <list>)))
    0;
  else
    count-anywhere(item, first(tree)) + count-anywhere(item, tail(tree));
  end if;
end method count-anywhere;

//  ==============================
define method dot-product (a, b)
  // Compute the mathematical dot product of two vectors.
  if (empty?(a) | empty?(b))
    0;
  else
    first(a) * first(b) + dot-product(tail(a), tail(b));
  end if;
end method dot-product;

define method dot-product (a, b)
  // Compute the mathematical dot product of two vectors.
  let sum = 0;
  for (i from 0 below size(a)) inc!(sum, a[i] * b[i]); end for;
  sum;
end method dot-product;

define method dot-product (a, b)
  // Compute the mathematical dot product of two vectors.
  apply(\+, map(\*, a, b));
end method dot-product;

//  ==============================
"eof";

