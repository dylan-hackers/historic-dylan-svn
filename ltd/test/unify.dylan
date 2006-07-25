//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File unify.lisp: Unification functions
requires("patmatch");

// Should we do the occurs check?
define variable *occurs-check* = #t;

define method unify (x, y, #key bindings = no-bindings)
  // See if x and y match with given bindings.
  if (bindings == fail)
    fail;
  elseif (x == y)
    bindings;
  elseif (variable-p(x))
    unify-variable(x, y, bindings);
  elseif (variable-p(y))
    unify-variable(y, x, bindings);
  elseif (instance?(x, <pair>) & instance?(y, <pair>))
    unify(tail(x), tail(y), unify(first(x), first(y), bindings));
  else
    fail;
  end if;
end method unify;

define method unify-variable (var, x, bindings)
  // Unify var with x, using (and maybe extending) bindings.
  if (get-binding(var, bindings))
    unify(lookup(var, bindings), x, bindings);
  elseif (variable-p(x) & get-binding(x, bindings))
    unify(var, lookup(x, bindings), bindings);
  elseif (*occurs-check* & occurs-check(var, x, bindings))
    fail;
  else
    extend-bindings(var, x, bindings);
  end if;
end method unify-variable;

define method occurs-check (var, x, bindings)
  // Does var occur anywhere inside x?
  if (var == x)
    #t;
  elseif (variable-p(x) & get-binding(x, bindings))
    occurs-check(var, lookup(x, bindings), bindings);
  elseif (instance?(x, <pair>))
    occurs-check(var, first(x), bindings)
     | occurs-check(var, tail(x), bindings);
  else
    #f;
  end if;
end method occurs-check;

//  ==============================
define method subst-bindings (bindings, x)
  // Substitute the value of variables in bindings into x,
  //   taking recursively bound variables into account.
  if (bindings == fail)
    fail;
  elseif (bindings == no-bindings)
    x;
  elseif (variable-p(x) & get-binding(x, bindings))
    subst-bindings(bindings, lookup(x, bindings));
  elseif (not(instance?(x, <list>)))
    x;
  else
    reuse-cons(subst-bindings(bindings, head(x)),
               subst-bindings(bindings, tail(x)), x);
  end if;
end method subst-bindings;

//  ==============================
define method unifier (x, y)
  // Return something that unifies with both x and y (or fail).
  subst-bindings(unify(x, y), x);
end method unifier;

