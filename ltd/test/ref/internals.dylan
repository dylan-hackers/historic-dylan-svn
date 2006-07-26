//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Internals.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

define module dtp export reset-dtp; end module dtp;

// ----------------------------------------------------------------------------
define method reset-dtp (#key only-internal = #f)
  begin
    do(// LTD: Function UNINTERN not yet implemented.
       unintern, *all-gensymed-variables*);
    *all-gensymed-variables*;
  end;
  reset-variables(only-internal: only-internal);
  reset-hierarchy();
  reset-database();
end method reset-dtp;

// ----------------------------------------------------------------------------
// LTD: No macros.
#"add-to-end";

// LTD: No macros.
#"add-to-end-if-new";

// LTD: No macros.
#"add-new-to-beginning";

// ----------------------------------------------------------------------------
define method find-vars (list)
  if (instance?(list, <pair>))
    concatenate(find-vars(head(list)), find-vars(tail(list)));
  elseif (varp(list))
    list(list);
  end if;
end method find-vars;

define method binding-list-vars (binding-list)
  cl-remove-duplicates(begin
                         let _acc = make(<deque>);
                         for (binding in binding-list)
                           push-last(_acc, head(binding));
                           push-last(_acc, tail(binding));
                         finally
                           _acc;
                         end for;
                       end);
end method binding-list-vars;

// ----------------------------------------------------------------------------
define method make-new-variable (var)
  let var-name = #f;
  var-name
   := copy-sequence(as(<string>, var), 0,
                    find-key(as(<string>, var), curry(\==, '_')));
  var-name := concatenate-as(<string>, var-name, "_");
  push!(generate-symbol(#"string"(var-name)), *all-gensymed-variables*);
  first(*all-gensymed-variables*);
end method make-new-variable;

// ----------------------------------------------------------------------------
define method variable-to-string (var)
  // Simplify unless *SHOW-RENAMED-VARIABLES*
  if (*show-renamed-variables*)
    as(<string>, var);
  else
    copy-sequence(as(<string>, var), 0,
                  find-key(as(<string>, var), curry(\==, '_')));
  end if;
end method variable-to-string;

// ----------------------------------------------------------------------------
define method merge-binding-lists (binding-lists)
  // Merge with a Ginsberg trick: unify variable list with value list
  let bindings = #f;
  let new-bl = #f;
  bindings
   := reduce1(concatenate,
              map(method (bl)
                    remove(bl, #t,
                           test: method (x, y)
                                   x == binding-variable(y);
                                 end method);
                  end method,
                  binding-lists));
  new-bl
   := dtp-unifyp(map(binding-variable, bindings),
                 map(binding-value, bindings));
  if (new-bl)
    dtp-ify-binding-list(new-bl);
  else
    #"not-a-binding-list";
  end if;
end method merge-binding-lists;

define method dtp-ify-binding-list (binding-list)
  // Eliminate Ginsberg's (T . T) for success
  remove(binding-list, #t,
         test: method (x, y) x == binding-variable(y); end method);
end method dtp-ify-binding-list;

// ----------------------------------------------------------------------------
define method unify-collection (sexp, #rest more-sexps)
  if (more-sexps)
    let other-sexp = apply(unify-collection, more-sexps);
    let bl = #f;
    bl := dtp-unifyp(sexp, other-sexp);
    if (bl) plug(sexp, bl); end if;
  else
    sexp;
  end if;
end method unify-collection;

// ----------------------------------------------------------------------------
define method make-new-id (str, #key num = #f)
  if (num)
    as(<symbol>, format(#f, "%S-%D", str, num));
  else
    as(<symbol>, format(#f, "%S-%D", str, inc!(*node-id-count*)));
  end if;
end method make-new-id;

// ----------------------------------------------------------------------------
define method list-rename-variables (list)
  let vars = find-vars(list);
  let bl = #f;
  bl := map(method (x) pair(x, make-new-variable(x)); end method, vars);
  plug(list, bl);
end method list-rename-variables;

// ----------------------------------------------------------------------------
define method permutations (list-of-items)
  if (size(list-of-items) = 1)
    list(list-of-items);
  else
    let _acc = #();
    for (item in list-of-items,
         remaining = remove(list-of-items, item,
                            test: \=) then remove(list-of-items,
                                                  item,
                                                  test: \=))
      _acc
       := concatenate(_acc,
                      begin
                        let _acc = make(<deque>);
                        for (perm in permutations(remaining))
                          push-last(_acc, pair(item, perm));
                        finally
                          _acc;
                        end for;
                      end);
    finally
      _acc;
    end for;
  end if;
end method permutations;

// ----------------------------------------------------------------------------
define method tree-find (item, tree)
  if (empty?(tree) | not(instance?(tree, <list>)))
    #f;
  elseif (cl-find(item, tree))
    #t;
  else
    any?(method (x) tree-find(item, x); end method, tree);
  end if;
end method tree-find;

// ----------------------------------------------------------------------------
define method partition (list, test)
  // Return list with items satisfying TEST first, others following
  let best = #f;
  let others = #f;
  best := remove(list, complement(test));
  others := choose(complement(test), list);
  concatenate(best, others);
end method partition;

// ----------------------------------------------------------------------------
define method tree-depth (list, #key prior-depth = 0)
  // Maximum depth of list structure
  if (instance?(list, <pair>))
    reduce1(max,
            map(method (l) tree-depth(l, prior-depth + 1); end method, list));
  else
    prior-depth;
  end if;
end method tree-depth;

// ----------------------------------------------------------------------------
define method set-equal (set-1, set-2, #key test = \=)
  ~ // LTD: Function SET-EXCLUSIVE-OR not yet implemented.
    set-exclusive-or(set-1, set-2, test: test);
end method set-equal;

// ----------------------------------------------------------------------------
define method list-of-length-one-p (list)
  first(list) & empty?(tail(list));
end method list-of-length-one-p;

define method list-of-length-more-than-one? (list)
  tail(list);
end method list-of-length-more-than-one?;

// ----------------------------------------------------------------------------
"eof";

