//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File auxfns.lisp: Auxiliary functions used by all other programs
//  Load this file before running any other programs.
#f;

//  The function REQUIRES is used in subsequent files to state dependencies
//  between files.  The current definition just loads the required files,
//  assumming they match the pathname specified in *PAIP-source-files*.
//  You should change that to match where you have stored the files.
//  A more sophisticated REQUIRES would only load it if it has not yet
//  been loaded, and would search in different directories if needed.
//  First set up read macros for #P"pathname", if they are missing.
#"eval-when"(#"eval"(compile: #"load"),
             #"defun"(read-pathname: #"stream"(char: #"arg-count"),
                      #"declare"(#"ignore"(char: #"arg-count")),
                      #"parse-namestring"(#"read-preserving-whitespace"(stream: #"t",
                                                                        nil: #"t"))),
             #"set-dispatch-macro-character"('#', 'P', #"read-pathname"),
             #"set-dispatch-macro-character"('#', 'p', #"read-pathname"));

// The location of the source files for this book.
//   CHANGE IT TO REFLECT THE LOCATION OF THE FILES ON YOUR COMPUTER.
define variable *paip-source-files* = #P"*.lisp";

define method requires (#rest files)
  // The arguments are files that are required to run an application.
  fluid-bind (*default-pathname-defaults* = *paip-source-files*)
    begin
      do(// LTD: Function LOAD not yet implemented.
         load, files);
      files;
    end;
  end fluid-bind;
end method requires;

//  Macros (formerly in auxmacs.lisp: that file no longer needed)
begin
  // LTD: No macros.
  #"once-only";
  define method side-effect-free? (exp)
    // Is exp a constant, variable, or function,
    //   or of the form (THE type x) where x is side-effect-free?
    not(instance?(exp, <list>)) | constant?(exp)
     | starts-with(exp, #"function")
     | (starts-with(exp, #"the") & side-effect-free?(third(exp)));
  end method side-effect-free?;
  // LTD: No macros.
  #"funcall-if";
  // LTD: No macros.
  #"read-time-case";
  define method rest2 (x)
    // The rest of a list after the first TWO elements.
    tail(tail(x));
  end method rest2;
  define method find-anywhere (item, tree)
    // Does item occur anywhere in tree?
    if (not(instance?(tree, <list>)))
      if (item == tree) tree; end if;
    else
      find-anywhere(item, first(tree)) | find-anywhere(item, tail(tree));
    end if;
  end method find-anywhere;
  define method starts-with (list, x)
    // Is x a list whose first element is x?
    instance?(list, <pair>) & first(list) == x;
  end method starts-with;
end;

//  Auxiliary Functions
find-all-if
 := method (x, y, #rest r) apply(remove, y, complement(x), r); end method;

define method find-all (item, sequence, #rest keyword-args, #key test = \==,
                        test-not, #"#all-keys")
  // Find all those elements of sequence that match item,
  //   according to the keywords.  Doesn't alter sequence.
  if (test-not)
    apply(cl-remove, item, sequence, test-not: complement(test-not),
          keyword-args);
  else
    apply(cl-remove, item, sequence, test: complement(test), keyword-args);
  end if;
end method find-all;

define method partition-if (pred, list)
  // Return 2 values: elements of list that satisfy pred,
  //   and elements that don't.
  let yes-list = #f;
  let no-list = #f;
  for (item in list)
    if (pred(item)) push!(item, yes-list); else push!(item, no-list); end if;
  end for;
  values(reverse!(yes-list), reverse!(no-list));
end method partition-if;

define method maybe-add (op, exps, #key if-nil)
  // For example, (maybe-add 'and exps t) returns
  //   t if exps is nil, exps if there is only one,
  //   and (and exp1 exp2...) if there are several exps.
  if (empty?(exps))
    if-nil;
  elseif (length=1(exps))
    first(exps);
  else
    pair(op, exps);
  end if;
end method maybe-add;

//  ==============================
#"defun"(map-into: #"result-sequence"(function: #"&rest", #"sequences"),
         "Destructively set elements of RESULT-SEQUENCE to the results\n  of applying FUNCTION to respective elements of SEQUENCES.",
         #"let"((#"arglist"(#"make-list"(#"length"(#"sequences"))))(#"n"(#"if"(#"listp"(#"result-sequence"),
                                                                               most-positive-fixnum: #"array-dimension"(result-sequence: 0)))),
                //  arglist is made into a list of args for each call
                //  n is the length of the longest vector
                #"when"(sequences: #"setf"(n: #"min"(n: #"loop"(for: #"seq",
                                                                in: #"sequences",
                                                                minimize: #"length"(#"seq"))))),
                //  Define some shared functions:
                #"flet"((#"do-one-call"(#"i"(),
                                        #"loop"(for: #"seq",
                                                on: #"sequences",
                                                for: #"arg",
                                                on: #"arglist",
                                                do: #"if"(#"listp"(#"first"(#"seq")),
                                                          #"setf"(#"first"(#"arg"),
                                                                  #"pop"(#"first"(#"seq"))),
                                                          #"setf"(#"first"(#"arg"),
                                                                  #"aref"(#"first"(#"seq"),
                                                                          #"i")))),
                                        #"apply"(function: #"arglist")))(#"do-result"(#"i"(),
                                                                                      #"if"(#"and"(#"vectorp"(#"result-sequence"),
                                                                                                   #"array-has-fill-pointer-p"(#"result-sequence")),
                                                                                            #"setf"(#"fill-pointer"(#"result-sequence"),
                                                                                                    #"max"(i: #"fill-pointer"(#"result-sequence")))))),
                        #"declare"(#"inline"(#"do-one-call")),
                        //  Decide if the result is a list or vector,
                        //  and loop through each element
                        #"if"(#"listp"(#"result-sequence"),
                              #"loop"(for: #"i", from: 0, to: #"-"(n: 1),
                                      for: #"r", on: #"result-sequence",
                                      do: #"setf"(#"first"(#"r"),
                                                  #"do-one-call"(#"i")),
                                      finally: #"do-result"(#"i")),
                              #"loop"(for: #"i", from: 0, to: #"-"(n: 1),
                                      do: #"setf"(#"aref"(result-sequence: #"i"),
                                                  #"do-one-call"(#"i")),
                                      finally: #"do-result"(#"i")))),
                #"result-sequence"));

define method seq-ref (seq, index)
  // Return code that indexes into a sequence, using
  //   the pop-lists/aref-vectors strategy.
  list(#"if", list(#"listp", seq),
       list(#"prog1", list(#"first", seq),
            list(#"setq", seq, list(#"the", #"list", list(#"rest", seq)))),
       list(#"aref", seq, index));
end method seq-ref;

define method maybe-set-fill-pointer (array, new-length)
  // If this is an array with a fill pointer, set it to
  //   new-length, if that is longer than the current length.
  if (instance?(array, <array>) & array-has-fill-pointer-p(array))
    size(array) := max(size(array), new-length);
  end if;
end method maybe-set-fill-pointer;

//  Reduce
#"defun"(reduce*: #"fn"(seq: #"from-end", start: #"end", key: #"init",
                        #"init-p"),
         #"funcall"(#"if"(#"listp"(#"seq"), reduce-list: #"reduce-vect"),
                    fn: #"seq", from-end: #"or"(start: 0), end: #"key",
                    init: #"init-p"));

define method reduce (function, sequence, #key from-end, start, end, key,
                      initial-value = #f)
  reduce*(function, sequence, from-end, start, end, key, initial-value,
          initial-value-p);
end method reduce;

define method reduce-vect (fn, seq, from-end, start, end, key, init, init-p)
  if (empty?(end)) end := size(seq); end if;
  assert(0 <= start & start <= end & end <= size(seq));
  select (end - start)
    1
       => if (init-p)
            fn(init, funcall-if(key, seq[start]));
          else
            funcall-if(key, seq[start]);
          end if;
    0
       => if (init-p) init; else fn(); end if;
    otherwise
       => if (~ from-end)
            let result
                = if (init-p)
                    fn(init, funcall-if(key, seq[start]));
                  else
                    fn(funcall-if(key, seq[start]),
                       funcall-if(key, seq[start + 1]));
                  end if;
            for (i from start + if (init-p) 1; else 2; end if to end - 1)
              result := fn(result, funcall-if(key, seq[i]));
            end for;
            result;
          else
            let result
                = if (init-p)
                    fn(funcall-if(key, seq[end - 1]), init);
                  else
                    fn(funcall-if(key, seq[end - 2]),
                       funcall-if(key, seq[end - 1]));
                  end if;
            for (i from end - if (init-p) 2; else 3; end if to start)
              result := fn(funcall-if(key, seq[i]), result);
            end for;
            result;
          end if;
  end select;
end method reduce-vect;

define method reduce-list (fn, seq, from-end, start, end, key, init, init-p)
  if (empty?(end)) end := size(seq); end if;
  if (start > 0)
    reduce-list(fn, nth-tail(seq, start), from-end, 0, end - start, key, init,
                init-p);
  elseif (empty?(seq) | start == end)
    if (init-p) init; else fn(); end if;
  elseif (end - start = 1)
    if (init-p)
      fn(init, funcall-if(key, first(seq)));
    else
      funcall-if(key, first(seq));
    end if;
  elseif (from-end)
    reduce-vect(fn, as(<vector>, seq), #t, start, end, key, init, init-p);
  elseif (empty?(tail(seq)))
    if (init-p)
      fn(init, funcall-if(key, first(seq)));
    else
      funcall-if(key, first(seq));
    end if;
  else
    begin
      let result
          = if (init-p)
              fn(init, funcall-if(key, pop!(seq)));
            else
              fn(funcall-if(key, pop!(seq)), funcall-if(key, pop!(seq)));
            end if;
      if (end)
        for (% from 1 to end - if (init-p) 1; else 2; end if, while seq)
          result := fn(result, funcall-if(key, pop!(seq)));
        end for;
      else
        for (while seq)
          result := fn(result, funcall-if(key, pop!(seq)));
        end for;
      end if;
      result;
    end;
  end if;
end method reduce-list;

//  ==============================
//  Other:
//  NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
//  anything else) to a symbol in the common-lisp package is undefined.
//  Therefore, it would be best to rename the function symbol to something else.
//  This has not been done (for compatibility with the book).  The only near-ANSI
//  Lisp tested was Franz's Allegro EXCL, for which we allow the definition by
//  unlocking the excl and common-lisp packages with the following form:
//
nil(#f, nil(#f), "Concatenate symbols or strings to form an interned symbol",
    nil(nil(#f, "~{~a~}", #f)));

define method new-symbol (#rest args)
  // Concatenate symbols or strings to form an uninterned symbol
  as(<symbol>,
     (method (s, #rest args)
        apply(maybe-initiate-xp-printing,
              method (xp, #rest args)
                let args = pop!(args);
                block (return)
                  local method go-l ()
                          if (empty?(args)) return(#f); end if;
                          fluid-bind (*print-escape* = #f)
                            write+(pop!(args), xp);
                          end fluid-bind;
                          go-l();
                        end method go-l;
                  go-l();
                end block;
                if (args) copy-sequence(args); end if;
              end method,
              s, args);
      end method)(#f, args));
end method new-symbol;

define method last1 (list)
  // Return the last element (not last cons cell) of list
  first(copy-sequence(list, start: size(list) - 1));
end method last1;

//  ==============================
define method mappend (fn, list)
  // Append the results of calling fn on each element of list.
  //   Like mapcon, but uses append instead of nconc.
  apply(concatenate, map(fn, list));
end method mappend;

define method mklist (x)
  // If x is a list return it, otherwise return the list of x
  if (instance?(x, <list>)) x; else list(x); end if;
end method mklist;

define method flatten (exp)
  // Get rid of imbedded lists (to one level only).
  mappend(mklist, exp);
end method flatten;

define method random-elt (seq)
  // Pick a random element out of a sequence.
  seq[random-uniform(to: size(seq))];
end method random-elt;

//  ==============================
define method member-equal (item, list)
  member?(item, list, test: \=);
end method member-equal;

//  The Debugging Output Facility:
// Identifiers used by dbg
define variable *dbg-ids* = #f;

define method dbg (id, format-string, #rest args)
  // Print debugging info if (DEBUG ID) has been specified.
  if (member?(id, *dbg-ids*))
    write-element(*debug-io*, '\n');
    apply(format, *debug-io*, format-string, args);
  end if;
end method dbg;

define method debug (#rest ids)
  // Start dbg output on the given ids.
  *dbg-ids* := union(ids, *dbg-ids*);
end method debug;

define method undebug (#rest ids)
  // Stop dbg on the ids.  With no ids, stop dbg altogether.
  *dbg-ids*
   := if (empty?(ids)) #f; else set-difference(*dbg-ids*, ids); end if;
end method undebug;

//  ==============================
define method dbg-indent (id, indent, format-string, #rest args)
  // Print indented debugging info if (DEBUG ID) has been specified.
  if (member?(id, *dbg-ids*))
    write-element(*debug-io*, '\n');
    for (i from 0 below indent) print("  ", *debug-io*); end for;
    apply(format, *debug-io*, format-string, args);
  end if;
end method dbg-indent;

//  ==============================
define constant fail = #f;

define constant no-bindings = #(#(#"t" . #"t"));

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
  // Match a single variable against input in the context of the bindings
  if (get-binding(var, bindings))
    if (input = lookup(var, bindings)) bindings; else fail; end if;
  else
    extend-bindings(var, input, bindings);
  end if;
end method match-variable;

define method get-binding (var, bindings)
  cl-assoc(var, bindings);
end method get-binding;

define method lookup (var, bindings)
  tail(cl-assoc(var, bindings));
end method lookup;

define method extend-bindings (var, val, bindings)
  pair(pair(var, val), bindings);
end method extend-bindings;

define method variable-p (x)
  // Is x a variable (a symbol beginning with `?')?
  instance?(x, <symbol>) & as(<string>, x)[0] = '?';
end method variable-p;

//  ==============================
define method compose (#rest functions)
  method (x)
    reduce(// LTD: Can't convert complex function FUNCALL.
           funcall, x, functions);
  end method;
end method compose;

define method complement (fn)
  // If FN returns y, then (complement FN) returns (not y).
  method (#rest args) ~ apply(fn, args); end method;
end method complement;

//  ==============================
//  The Memoization facility:
// LTD: No macros.
#"defun-memo";

define method memo (fn, #key key = first, test = \==, name)
  // Return a memo-function of fn.
  let table = make(<table>, test: test);
  symbol-get-property(name, #"memo") := table;
  method (#rest args)
    let k = key(args);
    let (val, found-p) = table[k];
    if (found-p) val; else table[k] := apply(fn, args); end if;
  end method;
end method memo;

define method memoize (fn-name, #key key = first, test = \==)
  // Replace fn-name's global definition with a memoized version.
  clear-memoize(fn-name);
  fn-name := memo(fn-name, name: fn-name, key: key, test: test);
end method memoize;

define method clear-memoize (fn-name)
  // Clear the hash table from a memo function.
  let table = symbol-get-property(fn-name, #"memo");
  if (table) size(table) := 0; end if;
end method clear-memoize;

//  Delayed computation:
define class <delay> (<object>)
  slot delay-value, init-keyword: #"delay-value";
  slot delay-computed? = #f, init-keyword: #"delay-computed?";
end class <delay>;

// LTD: No macros.
#"delay";

define method force (delay)
  // Do a delayed computation, or fetch its previously-computed value.
  if (delay.delay-computed?)
    delay.delay-value;
  else
    let _ = delay.delay-value := (delay.delay-value)();
    delay.delay-computed? := #t;
    _;
  end if;
end method force;

//  Defresource:
// LTD: No macros.
#"defresource";

// LTD: No macros.
#"with-resource";

//  Queues:
//  A queue is a (last . contents) pair
define method queue-contents (q) tail(q); end method queue-contents;

define method make-queue ()
  // Build a new queue, with no elements.
  let q = pair(#f, #f);
  head(q) := q;
end method make-queue;

define method enqueue (item, q)
  // Insert item at the end of the queue.
  head(q) := (tail(head(q)) := pair(item, #f));
  q;
end method enqueue;

define method dequeue (q)
  // Remove an item from the front of the queue.
  pop!(tail(q));
  if (empty?(tail(q))) head(q) := q; end if;
  q;
end method dequeue;

define method front (q) first(queue-contents(q)); end method front;

define method empty-queue-p (q)
  empty?(queue-contents(q));
end method empty-queue-p;

define method queue-nconc (q, list)
  // Add the elements of LIST to the end of the queue.
  head(q)
   := begin
        let s93739 = (tail(head(q)) := list);
        copy-sequence(s93739, start: size(s93739) - 1);
      end;
end method queue-nconc;

//  Other:
define method sort* (seq, pred, #key key)
  // Sort without altering the sequence
  sort!(copy-sequence(seq),
        test: method (x, y) pred(key(x), key(y)); end method);
end method sort*;

define method reuse-cons (x, y, x-y)
  // Return (cons x y), or reuse x-y if it is equal to (cons x y)
  if (x == head(x-y) & y == tail(x-y)) x-y; else pair(x, y); end if;
end method reuse-cons;

//  ==============================
define method length=1 (x)
  // Is x a list of length 1?
  instance?(x, <pair>) & empty?(tail(x));
end method length=1;

define method rest3 (list)
  // The rest of a list after the first THREE elements.
  cdddr(list);
end method rest3;

//  ==============================
define method unique-find-if-anywhere (predicate, tree, #key found-so-far)
  // Return a list of leaves of tree satisfying predicate,
  //   with duplicates removed.
  if (not(instance?(tree, <list>)))
    if (predicate(tree)) add!(tree, found-so-far); else found-so-far; end if;
  else
    unique-find-if-anywhere(predicate, first(tree),
                            unique-find-if-anywhere(predicate,
                                                    tail(tree),
                                                    found-so-far));
  end if;
end method unique-find-if-anywhere;

define method find-if-anywhere (predicate, tree)
  // Does predicate apply to any atom in the tree?
  if (not(instance?(tree, <list>)))
    predicate(tree);
  else
    find-if-anywhere(predicate, first(tree))
     | find-if-anywhere(predicate, tail(tree));
  end if;
end method find-if-anywhere;

// LTD: No macros.
#"with-compilation-unit";

//  ==============================
// LTD: No macros.
#"define-enumerated-type";

//  ==============================
define method not-null (x) ~ empty?(x); end method not-null;

define method first-or-nil (x)
  // The first element of x if it is a list; else nil.
  if (instance?(x, <pair>)) first(x); else #f; end if;
end method first-or-nil;

define method first-or-self (x)
  // The first element of x, if it is a list; else x itself.
  if (instance?(x, <pair>)) first(x); else x; end if;
end method first-or-self;

//  ==============================
"eof";

