//  -*- Mode: Lisp; Syntax: Common-Lisp;  -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  krep1.lisp: Knowledge representation code; first version.
requires("prolog");

//  ==============================
//  An nlist is implemented as a (count . elements) pair:
define method make-empty-nlist ()
  // Create a new, empty nlist.
  pair(0, #f);
end method make-empty-nlist;

define method nlist-n (x)
  // The number of elements in an nlist.
  head(x);
end method nlist-n;

define method nlist-list (x)
  // The elements in an nlist.
  tail(x);
end method nlist-list;

define method nlist-push (item, nlist)
  // Add a new element to an nlist.
  inc!(head(nlist));
  push!(item, tail(nlist));
  nlist;
end method nlist-push;

//  ==============================
define class <dtree> (<object>)
  slot dtree-first = #f, init-keyword: #"dtree-first";
  slot dtree-rest = #f, init-keyword: #"dtree-rest";
  slot dtree-atoms = #f, init-keyword: #"dtree-atoms";
  slot dtree-var = make-empty-nlist(), init-keyword: #"dtree-var";
end class <dtree>;

//  ==============================
begin
  let predicates = #f;
  define method get-dtree (predicate)
    // Fetch (or make) the dtree for this predicate.
    let _that = #f;
    if (_that := symbol-get-property(predicate, #"dtree"))
      _that;
    else
      push!(predicate, predicates);
      symbol-get-property(predicate, #"dtree") := make-dtree();
    end if;
  end method get-dtree;
  define method clear-dtrees ()
    // Remove all the dtrees for all the predicates.
    for (predicate in predicates)
      symbol-get-property(predicate, #"dtree") := #f;
    end for;
    predicates := #f;
  end method clear-dtrees;
end;

//  ==============================
define method index (key)
  // Store key in a dtree node.  Key must be (predicate . args);
  //   it is stored in the predicate's dtree.
  dtree-index(key, key, get-dtree(predicate(key)));
end method index;

define method dtree-index (key, value, dtree)
  // Index value under all atoms of key in dtree.
  let _that = #f;
  if (instance?(key, <pair>))
    //  index on both first and rest
    dtree-index(first(key), value,
                dtree.dtree-first | (dtree.dtree-first := make-dtree()));
    dtree-index(tail(key), value,
                dtree.dtree-rest | (dtree.dtree-rest := make-dtree()));
  elseif (_that := empty?(key))
    _that;
    //  don't index on nil
    elseif (variable-p(key))
    //  index a variable
    nlist-push(value, dtree.dtree-var);
  else
    //  Make sure there is an nlist for this atom, and add to it
    nlist-push(value, lookup-atom(key, dtree));
  end if;
end method dtree-index;

define method lookup-atom (atom, dtree)
  // Return (or create) the nlist for this atom in dtree.
  lookup(atom, dtree.dtree-atoms)
   | begin
       let new = make-empty-nlist();
       push!(pair(atom, new), dtree.dtree-atoms);
       new;
     end;
end method lookup-atom;

//  ==============================
define method test-index ()
  let props
      = #(#(#"p", #"a", #"b"), #(#"p", #"a", #"c"), #(#"p", #"a", #"?x"),
          #(#"p", #"b", #"c"), #(#"p", #"b", #(#"f", #"c")),
          #(#"p", #"a", #(#"f" . #"?x")));
  clear-dtrees();
  begin do(index, props); props; end;
  print(list(props, get-dtree(#"p")), *standard-output*, circle: t, array: t,
        pretty: t);
  values();
end method test-index;

//  ==============================
define method fetch (query)
  // Return a list of buckets potentially matching the query,
  //   which must be a relation of form (predicate . args).
  dtree-fetch(query, get-dtree(predicate(query)), #f, 0, #f,
              $most-positive-fixnum);
end method fetch;

//  ==============================
define method dtree-fetch (pat, dtree, var-list-in, var-n-in, best-list,
                           best-n)
  // Return two values: a list-of-lists of possible matches to pat,
  //   and the number of elements in the list-of-lists.
  if (empty?(dtree) | empty?(pat) | variable-p(pat))
    values(best-list, best-n);
  else
    let var-nlist = dtree.dtree-var;
    let var-n = var-n-in + nlist-n(var-nlist);
    let var-list
        = if (empty?(nlist-list(var-nlist)))
            var-list-in;
          else
            pair(nlist-list(var-nlist), var-list-in);
          end if;
    if (var-n >= best-n)
      values(best-list, best-n);
    elseif (not(instance?(pat, <list>)))
      dtree-atom-fetch(pat, dtree, var-list, var-n, best-list, best-n);
    else
      let (list1, n1)
          = dtree-fetch(first(pat), dtree.dtree-first, var-list, var-n,
                        best-list, best-n);
      dtree-fetch(tail(pat), dtree.dtree-rest, var-list, var-n, list1, n1);
    end if;
  end if;
end method dtree-fetch;

define method dtree-atom-fetch (atom, dtree, var-list, var-n, best-list,
                                best-n)
  // Return the answers indexed at this atom (along with the vars),
  //   or return the previous best answer, if it is better.
  let atom-nlist = lookup(atom, dtree.dtree-atoms);
  if (empty?(atom-nlist) | empty?(nlist-list(atom-nlist)))
    values(var-list, var-n);
  elseif (atom-nlist & inc!(var-n, nlist-n(atom-nlist)) < best-n)
    values(pair(nlist-list(atom-nlist), var-list), var-n);
  else
    values(best-list, best-n);
  end if;
end method dtree-atom-fetch;

//  ==============================
#f;

define method mapc-retrieve (fn, query)
  // For every fact that matches the query,
  //   apply the function to the binding list.
  for (bucket in fetch(query))
    for (answer in bucket)
      let bindings = unify(query, answer);
      if (~ (bindings == fail)) fn(bindings); end if;
    end for;
  end for;
end method mapc-retrieve;

//  ==============================
define method retrieve (query)
  // Find all facts that match query.  Return a list of bindings.
  let answers = #f;
  mapc-retrieve(method (bindings) push!(bindings, answers); end method,
                query);
  answers;
end method retrieve;

define method retrieve-matches (query)
  // Find all facts that match query.
  //   Return a list of expressions that match the query.
  map(method (bindings) subst-bindings(bindings, query); end method,
      retrieve(query));
end method retrieve-matches;

//  ==============================
// LTD: No macros.
#"query-bind";

