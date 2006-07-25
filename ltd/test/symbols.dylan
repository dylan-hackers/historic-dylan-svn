//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
//  This file was stolen from Matt Ginsberg's MVL around 8/7/92
"(in-package dtp)";

// ----------------------------------------------------------------------------
//  Symbol-manipulation utilities.
//  MVL variables are of two types: normal variables, which begin with
//  the character "?" followed by any character other than a "*", and
//  sequence variables, which begin with "?*".
//  Variables are created using gentemp.  The functions that do that are
//  new-?var and new-*var.
//  Functions for creating and sensing variables.  The sensing functions
//  include checks to make sure that the argument is not simply the
//  one-character string "?" by making sure that it is at least two
//  characters long.
define method new-?var () generate-symbol(#"string"("?")); end method new-?var;

define method new-*var () generate-symbol(#"string"("?*")); end method new-*var;

//  To check to see if something is a variable, just check to see if the
//  first character is a "?".  To check the type, you have to be careful
//  to make sure you don't look at the second character of a symbol with
//  a 1-character name.
define method varp (x)
  instance?(x, <symbol>) & as(<string>, x)[0] = '?';
end method varp;

define method varp* (x)
  instance?(x, <symbol>) & (name := as(<string>, x))[0] = '?'
   & var-is-*(name);
end method varp*;

define method var-is-* (name)
  dimension(name, 0) > 1 & '*' = name[1];
end method var-is-*;

define method varp? (x)
  instance?(x, <symbol>) & (name := as(<string>, x))[0] = '?'
   & ~ var-is-*(name);
end method varp?;

//  vartype takes a symbol and returns ? if it is a normal varible, ?* if
//  it is a sequence variable, and NIL if it isn't a variable.
define method vartype (x)
  if (~ instance?(x, <symbol>))
    #f;
  elseif (/=(name := as(<string>, x)[0], '?'))
    #f;
  elseif (dimension(name, 0) = 1)
    #"?";
  elseif ('*' = name[1])
    #"?*";
  else
    #"?";
  end if;
end method vartype;

//  groundp takes a logical expression and sees if it is ground.  If it's
//  an atom, it's easy -- it's ground if it isn't a variable.  If it's an
//  s-exp, then you have to check to make sure every element is ground.
define method groundp (x)
  if (not(instance?(x, <list>))) ~ varp(x); else every?(groundp, x); end if;
end method groundp;

//  Finds and returns the list of variables in the expression p
define method vars-in (p, #key vars)
  if (empty?(p))
    vars;
  elseif (varp(p))
    vars := add!(p, vars);
  elseif (not(instance?(p, <list>)))
    vars;
  else
    vars-in(tail(p), vars-in(head(p), vars));
  end if;
end method vars-in;

//  Database variables are kept distinct from any variables that the user
//  types in.  They are not interned, and have *database-variable* in
//  their value slot.
define variable *database-variable* = list(#f);

define method database-variable (var)
  // LTD: Function BOUNDP not yet implemented.
  boundp(var)
   & var == *database-variable*;
end method database-variable;

//  standardize-variables takes a proposition and returns a new version
//  in which the variables have been standardized apart from all other
//  variables in the MVL database.  The way in which this is done is that
//  new variables are created that have the value *database-variable* and
//  have print names identical to those of the original variables (so that
//  the user can figure out what's going on).  Of course, when these
//  variables are actually displayed, they will frequently appear as
//  #:?var since they are uninterned.
//  standardize-variables returns two values -- the standardized version
//  of the sentence and a binding list.  It does the work by constructing a
//  binding list of the original variables and the standardized versions to
//  which they are bound.
define method standardize-variables (p)
  values(standardize-variables1(p), bdglist);
end method standardize-variables;

define method napcar (fn, list)
  for (items = list then cdr(items), until empty?(items))
    head(items) := fn(head(items));
  finally
    list;
  end for;
end method napcar;

//  Do a little bit of standardization.  If p is a cons, then just
//  standardize each variable in p.  Otherwise, if p is not a variable,
//  do nothing and return it.  If p is a variable that already appears in
//  the bdg list, then return its standardized version.  Otherwise, trim
//  the leading ? off the name of p and use concsym to make a new
//  uninterned version of it.
define method standardize-variables1 (p)
  if (instance?(p, <list>))
    map(standardize-variables1, p);
  else
    let var = vartype(p);
    let _that = #f;
    if (empty?(var))
      p;
    elseif (_that := tail(cl-assoc(p, bdglist)))
      _that;
    else
      var := as(<symbol>, as(<string>, p));
      var := *database-variable*;
      push!(pair(p, var), bdglist);
      var;
    end if;
  end if;
end method standardize-variables1;

//  symbol-maker takes a prefix and returns a function that itself returns
//  prefix0 prefix1 prefix2 ...  It also sets the value of the symbol to n
//  when called (which is needed for sorting).
define method symbol-maker (prefix)
  method () sym := counter; sym; end method;
end method symbol-maker;

define variable *proposition-prefix* = "P";

define variable proposition-maker = symbol-maker(*proposition-prefix*);

//  when putting something new into the MVL database, the new symbol
//  should begin with the proposition prefix and you should also take the
//  time now to label that symbol with a list of the variables in the
//  given expression. In some instances it is important *not* to rename
//  the variables in the given expression (probably because they've been
//  renamed already).  If uniquify is NIL, then no renaming is done.
define method new-meta-symbol (x, #key uniquify = #t)
  block (return)
    for (until %f)
      m := proposition-maker();
      if (~ symbol-plist(m)) return(#f); end if;
    end for;
  end block;
  if (uniquify)
    let (new, bdgs) = standardize-variables(x);
    begin
      symbol-get-property(m, #"denotes") := new;
      symbol-get-property(m, #"vars-in") := map(tail, bdgs);
      symbol-get-property(m, #"bdgs") := bdgs;
    end;
  else
    symbol-get-property(m, #"denotes") := x;
    symbol-get-property(m, #"vars-in") := vars-in(x);
    symbol-get-property(m, #"bdgs") := #f;
  end if;
  m;
end method new-meta-symbol;

//  concsym is a utility that takes two strings (typically a prefix like
//  "p" and an identifier like the proposition number), concatenates
//  them, interns the result if intern? is T, and returns the new symbol.
define method concsym (str1, str2)
  as(<symbol>, format(#f, "%S%S", str1, str2));
end method concsym;

//  The denotation of a sentence is on its denotes property.
define method denotes (x)
  symbol-get-property(x, #"denotes");
end method denotes;

//  the truth value assigned to a particular datum is on its truth-value
//  property.
define variable unknown = #f;

define method truth-value (x)
  instance?(x, <symbol>) & symbol-get-property(x, #"truth-value") | unknown;
end method truth-value;

