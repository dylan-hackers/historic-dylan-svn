//  The code in this file was mechanically extracted from the TeX
//  source files of On Lisp.  Some operators are multiply defined,
//  as they were in the book.  Usually this means just that you get
//  an upwardly compatible version 2 of whatever it is.  Note, though,
//  that if you load this whole file you get:
//   1. the cltl1 versions of alrec and atrec.
//   2. varsym? defined as needed by the Prolog compiler.  So if you
//      want to use e.g. match with variables that begin with question
//      marks, comment out the final definition of varsym?
//  If you have questions or comments about this code, or you want
//  something I didn't include, send mail to onlisp@das.harvard.edu.
#f;

#f;

define method last1 (lst)
  head(copy-sequence(lst, start: size(lst) - 1));
end method last1;

define method single (lst)
  instance?(lst, <pair>) & ~ tail(lst);
end method single;

define method append1 (lst, obj)
  concatenate(lst, list(obj));
end method append1;

define method conc1 (lst, obj) concatenate!(lst, list(obj)); end method conc1;

define method mklist (obj)
  if (instance?(obj, <list>)) obj; else list(obj); end if;
end method mklist;

define method longer (x, y)
  local method compare (x, y)
          instance?(x, <pair>) & (empty?(y) | compare(tail(x), tail(y)));
        end method compare;
  if (instance?(x, <list>) & instance?(y, <list>))
    compare(x, y);
  else
    size(x) > size(y);
  end if;
end method longer;

define method filter (fn, lst)
  let acc = #f;
  for (x in lst) let val = fn(x); if (val) push!(val, acc); end if; end for;
  reverse!(acc);
end method filter;

define method group (source, n)
  if (zero?(n)) error("zero length"); end if;
  local method rec (source, acc)
          let rest = nth-tail(source, n);
          if (instance?(rest, <pair>))
            rec(rest, pair(copy-sequence(source, 0, n), acc));
          else
            reverse!(pair(source, acc));
          end if;
        end method rec;
  if (source) rec(source, #f); else #f; end if;
end method group;

define method flatten (x)
  local method rec (x, acc)
          if (empty?(x))
            acc;
          elseif (not(instance?(x, <list>)))
            pair(x, acc);
          else
            rec(head(x), rec(tail(x), acc));
          end if;
        end method rec;
  rec(x, #f);
end method flatten;

define method prune (test, tree)
  local method rec (tree, acc)
          if (empty?(tree))
            reverse!(acc);
          elseif (instance?(head(tree), <pair>))
            rec(tail(tree), pair(rec(head(tree), #f), acc));
          else
            rec(tail(tree),
                if (test(head(tree)))
                  acc;
                else
                  pair(head(tree), acc);
                end if);
          end if;
        end method rec;
  rec(tree, #f);
end method prune;

define method find2 (fn, lst)
  if (empty?(lst))
    #f;
  else
    let val = fn(head(lst));
    if (val) values(head(lst), val); else find2(fn, tail(lst)); end if;
  end if;
end method find2;

define method before (x, y, lst, #key test = \==)
  lst
   & begin
       let first = head(lst);
       if (test(y, first))
         #f;
       elseif (test(x, first))
         lst;
       else
         before(x, y, tail(lst), test: test);
       end if;
     end;
end method before;

define method after (x, y, lst, #key test = \==)
  let rest = before(y, x, lst, test: test);
  rest & member?(x, rest, test: test);
end method after;

define method duplicate (obj, lst, #key test = \==)
  member?(obj, tail(member?(obj, lst, test: test)), test: test);
end method duplicate;

define method split-if (fn, lst)
  let acc = #f;
  for (src = lst then cdr(src), until empty?(src) | fn(head(src)))
    push!(head(src), acc);
  finally
    values(reverse!(acc), src);
  end for;
end method split-if;

define method most (fn, lst)
  if (empty?(lst))
    values(#f, #f);
  else
    let wins = head(lst);
    let max = fn(wins);
    for (obj in tail(lst))
      let score = fn(obj);
      if (score > max) begin wins := obj; max := score; end; end if;
    end for;
    values(wins, max);
  end if;
end method most;

define method best (fn, lst)
  if (empty?(lst))
    #f;
  else
    let wins = head(lst);
    for (obj in tail(lst)) if (fn(obj, wins)) wins := obj; end if; end for;
    wins;
  end if;
end method best;

define method mostn (fn, lst)
  if (empty?(lst))
    values(#f, #f);
  else
    let result = list(head(lst));
    let max = fn(head(lst));
    for (obj in tail(lst))
      let score = fn(obj);
      if (score > max)
        begin max := score; result := list(obj); end;
      elseif (score = max)
        push!(obj, result);
      end if;
    end for;
    values(reverse!(result), max);
  end if;
end method mostn;

define method map0-n (fn, n) mapa-b(fn, 0, n); end method map0-n;

define method map1-n (fn, n) mapa-b(fn, 1, n); end method map1-n;

define method mapa-b (fn, a, b, #key step = 1)
  for (i = a then i + step, result = nil then nil, until i > b)
    push!(fn(i), result);
  finally
    reverse!(result);
  end for;
end method mapa-b;

define method map-> (fn, start, test-fn, succ-fn)
  for (i = start then funcall(succ-fn, i), result = nil then nil,
       until test-fn(i))
    push!(fn(i), result);
  finally
    reverse!(result);
  end for;
end method map->;

define method mappend (fn, #rest lsts)
  apply(concatenate,
        apply(// LTD: Can't convert complex function MAPCAR.
              mapcar, fn, lsts));
end method mappend;

define method mapcars (fn, #rest lsts)
  let result = #f;
  for (lst in lsts) for (obj in lst) push!(fn(obj), result); end for; end for;
  reverse!(result);
end method mapcars;

define method rmapcar (fn, #rest args)
  if (any?(method (x) not(instance?(x, <list>)); end method, args))
    apply(fn, args);
  else
    apply(// LTD: Can't convert complex function MAPCAR.
          mapcar, method (#rest args) apply(rmapcar, fn, args); end method,
          args);
  end if;
end method rmapcar;

define method readlist (#rest args)
  values(// LTD: Function READ-FROM-STRING not yet implemented.
         read-from-string(concatenate-as(<string>,
                                         "(",
                                         apply(// LTD: Can't convert complex function READ-LINE.
                                               read-line,
                                               args),
                                         ")")));
end method readlist;

define method prompt (#rest args)
  apply(format, *query-io*, args);
  // LTD: Function READ not yet implemented.
  read(*query-io*);
end method prompt;

define method break-loop (fn, quit, #rest args)
  format(*query-io*, "Entering break-loop.\n");
  block (return)
    while (#t)
      let in = apply(prompt, args);
      if (quit(in))
        return(#f);
      else
        format(*query-io*, "%S\n", fn(in));
      end if;
    end while;
  end block;
end method break-loop;

define method mkstr (#rest args)
  let s = allocate-resource(#"string-output-simple-stream");
  #"character";
  for (a in args) print(a, s); end for;
  let _
      = // LTD: Function GET-OUTPUT-STREAM-STRING not yet implemented.
        get-output-stream-string(s);
  deallocate-resource(#"string-output-simple-stream", s);
  _;
end method mkstr;

define method symb (#rest args)
  values(as(<symbol>, apply(mkstr, args)));
end method symb;

define method reread (#rest args)
  values(// LTD: Function READ-FROM-STRING not yet implemented.
         read-from-string(apply(mkstr, args)));
end method reread;

define method explode (sym)
  map-as(<list>,
         method (c)
           as(<symbol>, make(<string>, size: 1, fill: c));
         end method,
         as(<string>, sym));
end method explode;

define variable *!equivs* = make(<table>);

define method ! (fn) *!equivs*[fn] | fn; end method !;

define method def! (fn, fn!) *!equivs*[fn] := fn!; end method def!;

define method memoize (fn)
  let cache = make(<table>, test: \=);
  method (#rest args)
    let (val, win) = cache[args];
    if (win) val; else cache[args] := apply(fn, args); end if;
  end method;
end method memoize;

define method compose (#rest fns)
  if (fns)
    let fn1 = head(copy-sequence(fns, start: size(fns) - 1));
    let fns = copy-sequence(fns, size(fns) - 1);
    method (#rest args)
      reduce(// LTD: Can't convert complex function FUNCALL.
             funcall, apply(fn1, args), fns);
    end method;
  else
    identity;
  end if;
end method compose;

define method fif (if, then, #key else)
  method (x)
    if (if (x); end if) then(x); elseif (else) else(x); end if;
  end method;
end method fif;

define method fint (fn, #rest fns)
  if (empty?(fns))
    fn;
  else
    let chain = apply(fint, fns);
    method (x) fn(x) & chain(x); end method;
  end if;
end method fint;

define method fun (fn, #rest fns)
  if (empty?(fns))
    fn;
  else
    let chain = apply(fun, fns);
    method (x) fn(x) | chain(x); end method;
  end if;
end method fun;

define method lrec (rec, #key base)
  local method self (lst)
          if (empty?(lst))
            if (instance?(base, <function>)) base(); else base; end if;
          else
            rec(head(lst), method () self(tail(lst)); end method);
          end if;
        end method self;
  self;
end method lrec;

define method rfind-if (fn, tree)
  if (not(instance?(tree, <list>)))
    fn(tree) & tree;
  else
    rfind-if(fn, head(tree))
     | if (tail(tree)) rfind-if(fn, tail(tree)); end if;
  end if;
end method rfind-if;

define method ttrav (rec, #key base = identity)
  local method self (tree)
          if (not(instance?(tree, <list>)))
            if (instance?(base, <function>)) base(tree); else base; end if;
          else
            rec(self(head(tree)), if (tail(tree)) self(tail(tree)); end if);
          end if;
        end method self;
  self;
end method ttrav;

define method trec (rec, #key base = identity)
  local method self (tree)
          if (not(instance?(tree, <list>)))
            if (instance?(base, <function>)) base(tree); else base; end if;
          else
            rec(tree, method () self(head(tree)); end method,
                method ()
                  if (tail(tree)) self(tail(tree)); end if;
                end method);
          end if;
        end method self;
  self;
end method trec;

// LTD: No macros.
#"mac";

// LTD: No macros.
#"when-bind";

// LTD: No macros.
#"when-bind*";

// LTD: No macros.
#"with-gensyms";

// LTD: No macros.
#"condlet";

define method condlet-clause (vars, cl, bodfn)
  bq-list(head(cl),
          bq-list(#"let", map(tail, vars),
                  bq-list(#"let", condlet-binds(vars, cl),
                          bq-cons(bodfn, map(tail, vars)))));
end method condlet-clause;

define method condlet-binds (vars, cl)
  map(method (bindform)
        if (instance?(bindform, <pair>))
          pair(tail(cl-assoc(head(bindform), vars)), tail(bindform));
        end if;
      end method,
      tail(cl));
end method condlet-binds;

// LTD: No macros.
#"if3";

// LTD: No macros.
#"nif";

// LTD: No macros.
#"in";

// LTD: No macros.
#"inq";

// LTD: No macros.
#"in-if";

// LTD: No macros.
#">case";

define method >casex (g, cl)
  let key = head(cl);
  let rest = tail(cl);
  if (instance?(key, <pair>))
    bq-cons(bq-list*(#"in", g, key), rest);
  elseif (inq(key, #t, otherwise))
    bq-cons(#"t", rest);
  else
    error("bad >case clause");
  end if;
end method >casex;

// LTD: No macros.
#"while";

// LTD: No macros.
#"till";

// LTD: No macros.
#"for";

// LTD: No macros.
#"do-tuples/o";

// LTD: No macros.
#"do-tuples/c";

define method dt-args (len, rest, src)
  map0-n(method (m)
           map1-n(method (n)
                    let x = m + n;
                    if (x >= len)
                      bq-list(#"nth", x - len, src);
                    else
                      bq-list(#"nth", x - 1, rest);
                    end if;
                  end method,
                  len);
         end method,
         len - 2);
end method dt-args;

// LTD: No macros.
#"mvdo*";

define method mvdo-gen (binds, rebinds, test, body)
  if (empty?(binds))
    let label = generate-symbol();
    bq-list*(#"prog", #(), label,
             bq-list(#"if", head(test),
                     bq-list(#"return", bq-cons(#"progn", tail(test)))),
             bq-append(body, mvdo-rebind-gen(rebinds),
                       bq-list(bq-list(#"go", label))));
  else
    let rec = mvdo-gen(tail(binds), rebinds, test, body);
    let var/s = head(head(binds));
    let expr = cadar(binds);
    if (not(instance?(var/s, <list>)))
      bq-list(#"let", bq-list(bq-list(var/s, expr)), rec);
    else
      bq-list(#"multiple-value-bind", var/s, expr, rec);
    end if;
  end if;
end method mvdo-gen;

define method mvdo-rebind-gen (rebinds)
  if (empty?(rebinds))
    #f;
  elseif (size(head(rebinds)) < 3)
    mvdo-rebind-gen(tail(rebinds));
  else
    pair(list(if (not(instance?(head(head(rebinds)), <list>)))
                #"setq";
              else
                #"multiple-value-setq";
              end if,
              head(head(rebinds)), third(head(rebinds))),
         mvdo-rebind-gen(tail(rebinds)));
  end if;
end method mvdo-rebind-gen;

// LTD: No macros.
#"mvpsetq";

define method shuffle (x, y)
  if (empty?(x))
    y;
  elseif (empty?(y))
    x;
  else
    apply(list, head(x), head(y), shuffle(tail(x), tail(y)));
  end if;
end method shuffle;

// LTD: No macros.
#"mvdo";

// LTD: No macros.
#"allf";

// LTD: No macros.
#"nilf";

// LTD: No macros.
#"tf";

// LTD: No macros.
#"toggle";

begin
  check-lock-definitions-compile-time(#"toggle2", #"function", #"defmacro",
                                      // LTD: Function FBOUNDP not yet implemented.
                                      fboundp(#"toggle2"));
  // LTD: Function MACRO-FUNCTION not yet implemented.
  macro-function(#"toggle2")
   := method (**macroarg**, ..environment..)
        dt-macro-argument-check(1, 1, **macroarg**, #"macro");
        let env = ..environment..;
        let g15931 = tail(**macroarg**);
        let %reference = car-fussy(g15931, #"%reference");
        let g15932
            = lambdascan-maxargs(0, tail(g15931),
                                 #(#"%reference", #"&environment", #"env"));
        #f;
        let (dummies, vals, newvals, setter, getter)
            = get-setf-expansion(%reference, env);
        for (d = dummies then cdr(d), v = vals then cdr(v),
             let-list = nil then cons(list(car(d), car(v)), let-list),
             until empty?(d))
          #f;
        finally
          bq-list(#"let*",
                  setf-binding-list(newvals, let-list,
                                    if (instance?(getter, <pair>)
                                         & #"the" == head(getter))
                                      list(#"the",
                                           second(getter),
                                           list(#"not", getter));
                                    else
                                      list(#"not", getter);
                                    end if),
                  setter);
        end for;
      end method;
  set-func_name(// LTD: Function MACRO-FUNCTION not yet implemented.
                macro-function(#"toggle2"),
                #"toggle2");
  .inv-func_formals(// LTD: Function FBOUNDP not yet implemented.
                    fboundp(#"toggle2"),
                    #(#"%reference"));
  ce-putprop(#"toggle2",
             method (**macroarg**, ..environment..)
               dt-macro-argument-check(1, 1, **macroarg**, #"macro");
               let env = ..environment..;
               let g15931 = tail(**macroarg**);
               let %reference = car-fussy(g15931, #"%reference");
               let g15932
                   = lambdascan-maxargs(0,
                                        tail(g15931),
                                        #(#"%reference",
                                          #"&environment",
                                          #"env"));
               #f;
               let (dummies, vals, newvals, setter, getter)
                   = get-setf-expansion(%reference, env);
               for (d = dummies then cdr(d), v = vals then cdr(v),
                    let-list = nil then cons(list(car(d), car(v)), let-list),
                    until empty?(d))
                 #f;
               finally
                 bq-list(#"let*",
                         setf-binding-list(newvals,
                                           let-list,
                                           if (instance?(getter, <pair>)
                                                & #"the" == head(getter))
                                           list(#"the",
                                                second(getter),
                                                list(#"not", getter));
                                           else
                                           list(#"not", getter);
                                           end if),
                         setter);
               end for;
             end method,
             #".compile-file-macro.");
  symbol-remove-property(#"toggle2", #"%fun-documentation");
  record-source-file(#"toggle2");
  #"toggle2";
end;

begin
  check-lock-definitions-compile-time(#"concf", #"function", #"defmacro",
                                      // LTD: Function FBOUNDP not yet implemented.
                                      fboundp(#"concf"));
  // LTD: Function MACRO-FUNCTION not yet implemented.
  macro-function(#"concf")
   := method (**macroarg**, ..environment..)
        dt-macro-argument-check(2, 2, **macroarg**, #"macro");
        let env = ..environment..;
        let g15933 = tail(**macroarg**);
        let %reference = car-fussy(g15933, #"%reference");
        let obj = car-fussy(tail(g15933), #"obj");
        let g15934
            = lambdascan-maxargs(0, tail(tail(g15933)),
                                 #(#"%reference", #"obj", #"&environment",
                                   #"env"));
        #f;
        let (dummies, vals, newvals, setter, getter)
            = get-setf-expansion(%reference, env);
        for (d = dummies then cdr(d), v = vals then cdr(v),
             let-list = nil then cons(list(car(d), car(v)), let-list),
             until empty?(d))
          #f;
        finally
          bq-list(#"let*",
                  setf-binding-list(newvals, let-list,
                                    if (instance?(getter, <pair>)
                                         & #"the" == head(getter))
                                      list(#"the",
                                           second(getter),
                                           list(#"nconc", getter, obj));
                                    else
                                      list(#"nconc", getter, obj);
                                    end if),
                  setter);
        end for;
      end method;
  set-func_name(// LTD: Function MACRO-FUNCTION not yet implemented.
                macro-function(#"concf"),
                #"concf");
  .inv-func_formals(// LTD: Function FBOUNDP not yet implemented.
                    fboundp(#"concf"),
                    #(#"%reference", #"obj"));
  ce-putprop(#"concf",
             method (**macroarg**, ..environment..)
               dt-macro-argument-check(2, 2, **macroarg**, #"macro");
               let env = ..environment..;
               let g15933 = tail(**macroarg**);
               let %reference = car-fussy(g15933, #"%reference");
               let obj = car-fussy(tail(g15933), #"obj");
               let g15934
                   = lambdascan-maxargs(0,
                                        tail(tail(g15933)),
                                        #(#"%reference",
                                          #"obj",
                                          #"&environment",
                                          #"env"));
               #f;
               let (dummies, vals, newvals, setter, getter)
                   = get-setf-expansion(%reference, env);
               for (d = dummies then cdr(d), v = vals then cdr(v),
                    let-list = nil then cons(list(car(d), car(v)), let-list),
                    until empty?(d))
                 #f;
               finally
                 bq-list(#"let*",
                         setf-binding-list(newvals,
                                           let-list,
                                           if (instance?(getter, <pair>)
                                                & #"the" == head(getter))
                                           list(#"the",
                                                second(getter),
                                                list(#"nconc", getter, obj));
                                           else
                                           list(#"nconc", getter, obj);
                                           end if),
                         setter);
               end for;
             end method,
             #".compile-file-macro.");
  symbol-remove-property(#"concf", #"%fun-documentation");
  record-source-file(#"concf");
  #"concf";
end;

begin
  check-lock-definitions-compile-time(#"conc1f", #"function", #"defmacro",
                                      // LTD: Function FBOUNDP not yet implemented.
                                      fboundp(#"conc1f"));
  // LTD: Function MACRO-FUNCTION not yet implemented.
  macro-function(#"conc1f")
   := method (**macroarg**, ..environment..)
        dt-macro-argument-check(2, 2, **macroarg**, #"macro");
        let env = ..environment..;
        let g15935 = tail(**macroarg**);
        let %reference = car-fussy(g15935, #"%reference");
        let obj = car-fussy(tail(g15935), #"obj");
        let g15936
            = lambdascan-maxargs(0, tail(tail(g15935)),
                                 #(#"%reference", #"obj", #"&environment",
                                   #"env"));
        #f;
        let (dummies, vals, newvals, setter, getter)
            = get-setf-expansion(%reference, env);
        for (d = dummies then cdr(d), v = vals then cdr(v),
             let-list = nil then cons(list(car(d), car(v)), let-list),
             until empty?(d))
          #f;
        finally
          bq-list(#"let*",
                  setf-binding-list(newvals, let-list,
                                    if (instance?(getter, <pair>)
                                         & #"the" == head(getter))
                                      list(#"the",
                                           second(getter),
                                           list(#(#"lambda",
                                                  #(#"place", #"obj"),
                                                  #(#"nconc",
                                                    #"place",
                                                    #(#"list", #"obj"))),
                                                getter,
                                                obj));
                                    else
                                      list(#(#"lambda",
                                             #(#"place", #"obj"),
                                             #(#"nconc",
                                               #"place",
                                               #(#"list", #"obj"))),
                                           getter,
                                           obj);
                                    end if),
                  setter);
        end for;
      end method;
  set-func_name(// LTD: Function MACRO-FUNCTION not yet implemented.
                macro-function(#"conc1f"),
                #"conc1f");
  .inv-func_formals(// LTD: Function FBOUNDP not yet implemented.
                    fboundp(#"conc1f"),
                    #(#"%reference", #"obj"));
  ce-putprop(#"conc1f",
             method (**macroarg**, ..environment..)
               dt-macro-argument-check(2, 2, **macroarg**, #"macro");
               let env = ..environment..;
               let g15935 = tail(**macroarg**);
               let %reference = car-fussy(g15935, #"%reference");
               let obj = car-fussy(tail(g15935), #"obj");
               let g15936
                   = lambdascan-maxargs(0,
                                        tail(tail(g15935)),
                                        #(#"%reference",
                                          #"obj",
                                          #"&environment",
                                          #"env"));
               #f;
               let (dummies, vals, newvals, setter, getter)
                   = get-setf-expansion(%reference, env);
               for (d = dummies then cdr(d), v = vals then cdr(v),
                    let-list = nil then cons(list(car(d), car(v)), let-list),
                    until empty?(d))
                 #f;
               finally
                 bq-list(#"let*",
                         setf-binding-list(newvals,
                                           let-list,
                                           if (instance?(getter, <pair>)
                                                & #"the" == head(getter))
                                           list(#"the",
                                                second(getter),
                                                list(#(#"lambda",
                                                       #(#"place", #"obj"),
                                                       #(#"nconc",
                                                         #"place",
                                                         #(#"list", #"obj"))),
                                                     getter,
                                                     obj));
                                           else
                                           list(#(#"lambda",
                                                  #(#"place", #"obj"),
                                                  #(#"nconc",
                                                    #"place",
                                                    #(#"list", #"obj"))),
                                                getter,
                                                obj);
                                           end if),
                         setter);
               end for;
             end method,
             #".compile-file-macro.");
  symbol-remove-property(#"conc1f", #"%fun-documentation");
  record-source-file(#"conc1f");
  #"conc1f";
end;

begin
  check-lock-definitions-compile-time(#"concnew", #"function", #"defmacro",
                                      // LTD: Function FBOUNDP not yet implemented.
                                      fboundp(#"concnew"));
  // LTD: Function MACRO-FUNCTION not yet implemented.
  macro-function(#"concnew")
   := method (**macroarg**, ..environment..)
        dt-macro-argument-check(2, #f, **macroarg**, #"macro");
        let env = ..environment..;
        let g15937 = tail(**macroarg**);
        let %reference = car-fussy(g15937, #"%reference");
        let obj = car-fussy(tail(g15937), #"obj");
        let args = tail(tail(g15937));
        #f;
        let (dummies, vals, newvals, setter, getter)
            = get-setf-expansion(%reference, env);
        for (d = dummies then cdr(d), v = vals then cdr(v),
             let-list = nil then cons(list(car(d), car(v)), let-list),
             until empty?(d))
          #f;
        finally
          bq-list(#"let*",
                  setf-binding-list(newvals, let-list,
                                    if (instance?(getter, <pair>)
                                         & #"the" == head(getter))
                                      list(#"the",
                                           second(getter),
                                           apply(list,
                                                 #(#"lambda",
                                                   #(#"place",
                                                     #"obj",
                                                     #"&rest",
                                                     #"args"),
                                                   #(#"unless",
                                                     #(#"apply",
                                                       #(#"function",
                                                         #"member"),
                                                       #"obj",
                                                       #"place",
                                                       #"args"),
                                                     #(#"nconc",
                                                       #"place",
                                                       #(#"list", #"obj")))),
                                                 getter,
                                                 obj,
                                                 args));
                                    else
                                      apply(list,
                                            #(#"lambda",
                                              #(#"place",
                                                #"obj",
                                                #"&rest",
                                                #"args"),
                                              #(#"unless",
                                                #(#"apply",
                                                  #(#"function", #"member"),
                                                  #"obj",
                                                  #"place",
                                                  #"args"),
                                                #(#"nconc",
                                                  #"place",
                                                  #(#"list", #"obj")))),
                                            getter,
                                            obj,
                                            args);
                                    end if),
                  setter);
        end for;
      end method;
  set-func_name(// LTD: Function MACRO-FUNCTION not yet implemented.
                macro-function(#"concnew"),
                #"concnew");
  .inv-func_formals(// LTD: Function FBOUNDP not yet implemented.
                    fboundp(#"concnew"),
                    #(#"%reference", #"obj", #"&rest", #"args"));
  ce-putprop(#"concnew",
             method (**macroarg**, ..environment..)
               dt-macro-argument-check(2, #f, **macroarg**, #"macro");
               let env = ..environment..;
               let g15937 = tail(**macroarg**);
               let %reference = car-fussy(g15937, #"%reference");
               let obj = car-fussy(tail(g15937), #"obj");
               let args = tail(tail(g15937));
               #f;
               let (dummies, vals, newvals, setter, getter)
                   = get-setf-expansion(%reference, env);
               for (d = dummies then cdr(d), v = vals then cdr(v),
                    let-list = nil then cons(list(car(d), car(v)), let-list),
                    until empty?(d))
                 #f;
               finally
                 bq-list(#"let*",
                         setf-binding-list(newvals,
                                           let-list,
                                           if (instance?(getter, <pair>)
                                                & #"the" == head(getter))
                                           list(#"the",
                                                second(getter),
                                                apply(list,
                                                      #(#"lambda",
                                                        #(#"place",
                                                          #"obj",
                                                          #"&rest",
                                                          #"args"),
                                                        #(#"unless",
                                                          #(#"apply",
                                                            #(#"function",
                                                              #"member"),
                                                            #"obj",
                                                            #"place",
                                                            #"args"),
                                                          #(#"nconc",
                                                            #"place",
                                                            #(#"list",
                                                              #"obj")))),
                                                      getter,
                                                      obj,
                                                      args));
                                           else
                                           apply(list,
                                                 #(#"lambda",
                                                   #(#"place",
                                                     #"obj",
                                                     #"&rest",
                                                     #"args"),
                                                   #(#"unless",
                                                     #(#"apply",
                                                       #(#"function",
                                                         #"member"),
                                                       #"obj",
                                                       #"place",
                                                       #"args"),
                                                     #(#"nconc",
                                                       #"place",
                                                       #(#"list", #"obj")))),
                                                 getter,
                                                 obj,
                                                 args);
                                           end if),
                         setter);
               end for;
             end method,
             #".compile-file-macro.");
  symbol-remove-property(#"concnew", #"%fun-documentation");
  record-source-file(#"concnew");
  #"concnew";
end;

// LTD: No macros.
#"_f";

// LTD: No macros.
#"pull";

// LTD: No macros.
#"pull-if";

// LTD: No macros.
#"popn";

// LTD: No macros.
#"sortf";

// LTD: No macros.
#"aif";

// LTD: No macros.
#"awhen";

// LTD: No macros.
#"awhile";

// LTD: No macros.
#"aand";

// LTD: No macros.
#"acond";

// LTD: No macros.
#"alambda";

// LTD: No macros.
#"ablock";

// LTD: No macros.
#"aif2";

// LTD: No macros.
#"awhen2";

// LTD: No macros.
#"awhile2";

// LTD: No macros.
#"acond2";

begin
  let g = generate-symbol();
  define method read2 (#key str = *standard-input*)
    let val
        = // LTD: Function READ not yet implemented.
          read(str, #f, g);
    if (~ (val = g)) values(val, #t); end if;
  end method read2;
end;

// LTD: No macros.
#"do-file";

// LTD: No macros.
#"fn";

define method rbuild (expr)
  if (not(instance?(expr, <list>)) | head(expr) == #"lambda")
    expr;
  elseif (head(expr) == #"compose")
    build-compose(tail(expr));
  else
    build-call(head(expr), tail(expr));
  end if;
end method rbuild;

define method build-call (op, fns)
  let g = generate-symbol();
  bq-list(#"lambda", bq-list(g),
          bq-cons(op,
                  map(method (f) bq-list(rbuild(f), g); end method, fns)));
end method build-call;

define method build-compose (fns)
  let g = generate-symbol();
  bq-list(#"lambda", bq-list(g),
          local method rec (fns)
                  if (fns)
                    bq-list(rbuild(head(fns)), rec(tail(fns)));
                  else
                    g;
                  end if;
                end method rec;
            rec(fns));
end method build-compose;

// LTD: No macros.
#"alrec";

// LTD: No macros.
#"alrec";

// LTD: No macros.
#"on-cdrs";

define method unions (#rest sets)
  on-cdrs(union(it, rec), head(sets), tail(sets));
end method unions;

define method intersections (#rest sets)
  if (~ any?(empty?, sets))
    on-cdrs(intersection(it, rec), head(sets), tail(sets));
  end if;
end method intersections;

define method differences (set, #rest outs)
  on-cdrs(set-difference(rec, it), set, outs);
end method differences;

define method maxmin (args)
  if (args)
    on-cdrs(let (mx, mn) = rec;
              values(max(mx, it), min(mn, it)),
              values(head(args), head(args)), tail(args));
  end if;
end method maxmin;

// LTD: No macros.
#"atrec";

// LTD: No macros.
#"atrec";

// LTD: No macros.
#"on-trees";

define constant unforced = generate-symbol();

define class <delay> (<object>)
  slot delay-forced, init-keyword: #"delay-forced";
  slot delay-closure, init-keyword: #"delay-closure";
end class <delay>;

// LTD: No macros.
#"delay";

define method force (x)
  if (delay-p(x))
    if (x.delay-forced == unforced)
      (x.delay-closure)();
    else
      x.delay-forced;
    end if;
  else
    x;
  end if;
end method force;

// LTD: No macros.
#"abbrev";

// LTD: No macros.
#"abbrevs";

// LTD: No macros.
#"propmacro";

// LTD: No macros.
#"propmacros";

// LTD: No macros.
#"defanaph";

define method anaphex (args, expr)
  if (args)
    let sym = generate-symbol();
    bq-list(#"let*", bq-list(bq-list(sym, head(args)), bq-list(#"it", sym)),
            anaphex(tail(args), concatenate(expr, list(sym))));
  else
    expr;
  end if;
end method anaphex;

define method pop-symbol (sym)
  as(<symbol>, copy-sequence(as(<string>, sym), 1));
end method pop-symbol;

// LTD: No macros.
#"defanaph";

define method anaphex1 (args, call)
  if (args)
    let sym = generate-symbol();
    bq-list(#"let*", bq-list(bq-list(sym, head(args)), bq-list(#"it", sym)),
            anaphex1(tail(args), concatenate(call, list(sym))));
  else
    call;
  end if;
end method anaphex1;

define method anaphex2 (op, args)
  bq-list(#"let", bq-list(bq-list(#"it", head(args))),
          bq-list*(op, #"it", tail(args)));
end method anaphex2;

define method anaphex3 (op, args)
  bq-list(#"_f",
          bq-list(#"lambda", #(#"it"), bq-list*(op, #"it", tail(args))),
          head(args));
end method anaphex3;

// LTD: No macros.
#"defdelim";

begin
  let rpar
      = // LTD: Function GET-MACRO-CHARACTER not yet implemented.
        get-macro-character(')');
  define method ddfn (left, right, fn)
    // LTD: Function SET-MACRO-CHARACTER not yet implemented.
    set-macro-character(right, rpar);
    set-dispatch-macro-character('#', left,
                                 method (stream, char1, char2)
                                   apply(fn,
                                         // LTD: Function READ-DELIMITED-LIST not yet implemented.
                                         read-delimited-list(right,
                                                             stream,
                                                             #t));
                                 end method);
  end method ddfn;
end;

// LTD: No macros.
#"dbind";

define method destruc (pat, seq,
                       #key atom?
                             = method (x)
                                 not(instance?(x, <list>));
                               end method,
                       n = 0)
  if (empty?(pat))
    #f;
  else
    let rest
        = if (atom?(pat))
            pat;
          elseif (head(pat) == #"&rest")
            second(pat);
          elseif (head(pat) == #"&body")
            second(pat);
          else
            #f;
          end if;
    if (rest)
      bq-list(bq-list(rest, bq-list(#"subseq", seq, n)));
    else
      let p = head(pat);
      let rec = destruc(tail(pat), seq, atom?, n + 1);
      if (atom?(p))
        pair(bq-list(p, bq-list(#"elt", seq, n)), rec);
      else
        let var = generate-symbol();
        pair(pair(bq-list(var, bq-list(#"elt", seq, n)),
                  destruc(p, var, atom?)),
             rec);
      end if;
    end if;
  end if;
end method destruc;

define method dbind-ex (binds, body)
  if (empty?(binds))
    bq-cons(#"progn", body);
  else
    bq-list(#"let",
            map(method (b)
                  if (instance?(head(b), <pair>)) head(b); else b; end if;
                end method,
                binds),
            dbind-ex(apply(concatenate!,
                           map(method (b)
                                 if (instance?(head(b), <pair>))
                                   tail(b);
                                 end if;
                               end method,
                               binds)),
                     body));
  end if;
end method dbind-ex;

// LTD: No macros.
#"with-matrix";

// LTD: No macros.
#"with-array";

// LTD: No macros.
#"with-struct";

// LTD: No macros.
#"with-places";

define method wplac-ex (binds, body)
  if (empty?(binds))
    bq-cons(#"progn", body);
  else
    bq-list(#"symbol-macrolet",
            map(method (b)
                  if (instance?(head(b), <pair>)) head(b); else b; end if;
                end method,
                binds),
            wplac-ex(apply(concatenate!,
                           map(method (b)
                                 if (instance?(head(b), <pair>))
                                   tail(b);
                                 end if;
                               end method,
                               binds)),
                     body));
  end if;
end method wplac-ex;

define method match (x, y, #key binds)
  acond2((x == y | x == #"_" | y == #"_")(values(binds, #t)),
         (binding(x, binds))(match(it, y, binds)),
         (binding(y, binds))(match(x, it, binds)),
         (varsym?(x))(values(pair(pair(x, y), binds), #t)),
         (varsym?(y))(values(pair(pair(y, x), binds), #t)),
         (instance?(x, <pair>) & instance?(y, <pair>)
           & match(head(x), head(y), binds))(match(tail(x), tail(y), it)),
         t(values(#f, #f)));
end method match;

define method varsym? (x)
  instance?(x, <symbol>) & as(<string>, x)[0] == '?';
end method varsym?;

define method binding (x, binds)
  local method recbind (x, binds)
          aif(cl-assoc(x, binds), recbind(tail(it), binds) | it);
        end method recbind;
  let b = recbind(x, binds);
  values(tail(b), b);
end method binding;

// LTD: No macros.
#"if-match";

define method vars-in (expr,
                       #key atom?
                             = method (x)
                                 not(instance?(x, <list>));
                               end method)
  if (atom?(expr))
    if (var?(expr)) list(expr); end if;
  else
    union(vars-in(head(expr), atom?), vars-in(tail(expr), atom?));
  end if;
end method vars-in;

define method var? (x)
  instance?(x, <symbol>) & as(<string>, x)[0] == '?';
end method var?;

define method abab (seq)
  if-match(?x(?y, ?x, ?y), seq, values(?x, ?y), #f);
end method abab;

// LTD: No macros.
#"if-match";

// LTD: No macros.
#"pat-match";

define method simple? (x)
  not(instance?(x, <list>)) | head(x) == #"quote";
end method simple?;

define method gen-match (refs, then, else)
  if (empty?(refs))
    then;
  else
    let then = gen-match(tail(refs), then, else);
    if (simple?(head(head(refs))))
      match1(refs, then, else);
    else
      gen-match(head(refs), then, else);
    end if;
  end if;
end method gen-match;

define method match1 (refs, then, else)
  dbind((pat(expr))(rest), refs,
        if (gensym?(pat))
          bq-list(#"let", bq-list(bq-list(pat, expr)),
                  bq-list(#"if",
                          bq-list(#"and",
                                  bq-list*(#"typep",
                                           pat,
                                           #(#(#"quote", #"sequence"))),
                                  length-test(pat, rest)),
                          then, else));
        elseif (pat == #"_")
          then;
        elseif (var?(pat))
          begin
            let ge = generate-symbol();
            bq-list(#"let", bq-list(bq-list(ge, expr)),
                    bq-list(#"if",
                            bq-list(#"or", bq-list(#"gensym?", pat),
                                    bq-list(#"equal", pat, ge)),
                            bq-list(#"let", bq-list(bq-list(pat, ge)), then),
                            else));
          end;
        else
          bq-list(#"if", bq-list(#"equal", pat, expr), then, else);
        end if);
end method match1;

define method gensym? (s)
  instance?(s, <symbol>)
   & ~ // LTD: Function SYMBOL-PACKAGE not yet implemented.
       symbol-package(s);
end method gensym?;

define method length-test (pat, rest)
  let fin = caadar(copy-sequence(rest, start: size(rest) - 1));
  if (instance?(fin, <pair>) | fin == #"elt")
    bq-list(#"=", bq-list(#"length", pat), size(rest));
  else
    bq-list(#">", bq-list(#"length", pat), size(rest) - 2);
  end if;
end method length-test;

define method make-db (#key size = 100)
  make(<table>, size: size);
end method make-db;

define variable *default-db* = make-db();

define method clear-db (#key db = *default-db*)
  size(db) := 0;
end method clear-db;

// LTD: No macros.
#"db-query";

define method db-push (key, val, #key db = *default-db*)
  push!(val, db-query(key, db));
end method db-push;

// LTD: No macros.
#"fact";

// LTD: No macros.
#"with-answer";

define method interpret-query (expr, #key binds)
  select (car(expr))
    #"and"
       => interpret-and(reverse(tail(expr)), binds);
    #"or"
       => interpret-or(tail(expr), binds);
    #"not"
       => interpret-not(second(expr), binds);
    otherwise
       => lookup(head(expr), tail(expr), binds);
  end select;
end method interpret-query;

define method interpret-and (clauses, binds)
  if (empty?(clauses))
    list(binds);
  else
    apply(concatenate!,
          map(method (b) interpret-query(head(clauses), b); end method,
              interpret-and(tail(clauses), binds)));
  end if;
end method interpret-and;

define method interpret-or (clauses, binds)
  apply(concatenate!,
        map(method (c) interpret-query(c, binds); end method, clauses));
end method interpret-or;

define method interpret-not (clause, binds)
  if (interpret-query(clause, binds)) #f; else list(binds); end if;
end method interpret-not;

define method lookup (pred, args, #key binds)
  apply(concatenate!,
        map(method (x) aif2(match(x, args, binds), list(it)); end method,
            db-query(pred)));
end method lookup;

// LTD: No macros.
#"with-answer";

define method compile-query (q, body)
  select (car(q))
    #"and"
       => compile-and(tail(q), body);
    #"or"
       => compile-or(tail(q), body);
    #"not"
       => compile-not(second(q), body);
    #"lisp"
       => bq-list(#"if", second(q), body);
    otherwise
       => compile-simple(q, body);
  end select;
end method compile-query;

define method compile-simple (q, body)
  let fact = generate-symbol();
  bq-list(#"dolist",
          bq-list(fact, bq-list(#"db-query", bq-list(#"quote", head(q)))),
          bq-list*(#"pat-match", tail(q), fact, body, #(#())));
end method compile-simple;

define method compile-and (clauses, body)
  if (empty?(clauses))
    body;
  else
    compile-query(head(clauses), compile-and(tail(clauses), body));
  end if;
end method compile-and;

define method compile-or (clauses, body)
  if (empty?(clauses))
    #f;
  else
    let gbod = generate-symbol();
    let vars = vars-in(body, simple?);
    bq-list*(#"labels", bq-list(bq-list(gbod, vars, body)),
             map(method (cl)
                   compile-query(cl, bq-cons(gbod, vars));
                 end method,
                 clauses));
  end if;
end method compile-or;

define method compile-not (q, body)
  let tag = generate-symbol();
  bq-list(#"if",
          bq-list*(#"block", tag,
                   compile-query(q, bq-list*(#"return-from", tag, #(#()))),
                   #(#"t")),
          body);
end method compile-not;

*cont* := identity;

// LTD: No macros.
#"=lambda";

// LTD: No macros.
#"=defun";

// LTD: No macros.
#"=bind";

// LTD: No macros.
#"=values";

// LTD: No macros.
#"=funcall";

// LTD: No macros.
#"=apply";

define variable *paths* = #f;

define constant failsym = #"@";

// LTD: No macros.
#"choose";

// LTD: No macros.
#"choose-bind";

define method cb (fn, choices)
  if (choices)
    if (tail(choices))
      push!(method () cb(fn, tail(choices)); end method, *paths*);
    end if;
    fn(head(choices));
  else
    fail();
  end if;
end method cb;

define method fail ()
  if (*paths*) (pop!(*paths*))(); else failsym; end if;
end method fail;

define class <proc> (<object>)
  slot proc-pri, init-keyword: #"proc-pri";
  slot proc-state, init-keyword: #"proc-state";
  slot proc-wait, init-keyword: #"proc-wait";
end class <proc>;

#f;

define variable *halt* = generate-symbol();

define variable *default-proc* =
  make-proc(state: method (x)
                     format-out("\n>> ");
                     print(// LTD: Function EVAL not yet implemented.
                           eval(// LTD: Function READ not yet implemented.
                                read()),
                           *standard-output*);
                     pick-process();
                   end method);

// LTD: No macros.
#"fork";

// LTD: No macros.
#"program";

define method pick-process ()
  let (p, val) = most-urgent-process();
  begin *proc* := p; *procs* := remove!(*procs*, p); end;
  (p.proc-state)(val);
end method pick-process;

define method most-urgent-process ()
  let proc1 = *default-proc*;
  let max = -1;
  let val1 = #t;
  for (p in *procs*)
    let pri = p.proc-pri;
    if (pri > max)
      let val = ~ p.proc-wait | (p.proc-wait)();
      if (val) begin proc1 := p; max := pri; val1 := val; end; end if;
    end if;
  end for;
  values(proc1, val1);
end method most-urgent-process;

define method arbitrator (test, cont)
  begin *proc*.proc-state := cont; *proc*.proc-wait := test; end;
  push!(*proc*, *procs*);
  pick-process();
end method arbitrator;

// LTD: No macros.
#"wait";

// LTD: No macros.
#"yield";

define method setpri (n) *proc*.proc-pri := n; end method setpri;

define method halt (#key val)
  // LTD: Can't convert a run-time catch tag.
  *halt*(val);
end method halt;

define method kill (#key obj, #rest args)
  if (obj)
    *procs* := apply(cl-remove!, obj, *procs*, args);
  else
    pick-process();
  end if;
end method kill;

define variable *open-doors* = #f;

=defun(pedestrian, #(),
       wait(d, head(*open-doors*), format-out("Entering %S\n", d)));

program(ped, #(), fork(pedestrian(), 1));

=defun(capture, city(), take(city), setpri(1), yield(fortify(city)));

=defun(plunder, city(), loot(city), ransom(city));

define method take (c) format-out("Liberating %S.\n", c); end method take;

define method fortify (c) format-out("Rebuilding %S.\n", c); end method fortify;

define method loot (c) format-out("Nationalizing %S.\n", c); end method loot;

define method ransom (c) format-out("Refinancing %S.\n", c); end method ransom;

program(barbarians, #(), fork(capture(#"rome"), 100),
        fork(plunder(#"rome"), 98));

// LTD: No macros.
#"defnode";

// LTD: No macros.
#"down";

// LTD: No macros.
#"cat";

// LTD: No macros.
#"jump";

define method compile-cmds (cmds)
  if (empty?(cmds))
    #"regs";
  else
    bq-append(head(cmds), bq-list(compile-cmds(tail(cmds))));
  end if;
end method compile-cmds;

// LTD: No macros.
#"up";

// LTD: No macros.
#"getr";

// LTD: No macros.
#"set-register";

// LTD: No macros.
#"setr";

// LTD: No macros.
#"pushr";

// LTD: No macros.
#"with-parses";

define method types (word)
  select (word)
    (#"do", #"does", #"did")
       => #(#"aux", #"v");
    (#"time", #"times")
       => #(#"n", #"v");
    (#"fly", #"flies")
       => #(#"n", #"v");
    (#"like")
       => #(#"v", #"prep");
    (#"liked", #"likes")
       => #(#"v");
    (#"a", #"an", #"the")
       => #(#"det");
    (#"arrow", #"arrows")
       => #(#"n");
    (#"i", #"you", #"he", #"she", #"him", #"her", #"it")
       => #(#"pron");
    otherwise
       => #f;
  end select;
end method types;

defnode(mods, cat(n, mods/n, setr(mods, \*)));

defnode(mods/n, cat(n, mods/n, pushr(mods, \*)),
        up(bq-list(#"n-group", getr(mods))));

defnode(np, cat(det, np/det, setr(det, \*)), jump(np/det, setr(det, #f)),
        cat(pron, pron, setr(n, \*)));

defnode(pron, up(bq-list(#"np", bq-list(#"pronoun", getr(n)))));

defnode(np/det, down(mods, np/mods, setr(mods, \*)),
        jump(np/mods, setr(mods, #f)));

defnode(np/mods, cat(n, np/n, setr(n, \*)));

defnode(np/n,
        up(bq-list(#"np", bq-list(#"det", getr(det)),
                   bq-list(#"modifiers", getr(mods)),
                   bq-list(#"noun", getr(n)))),
        down(pp, np/pp, setr(pp, \*)));

defnode(np/pp,
        up(bq-list(#"np", bq-list(#"det", getr(det)),
                   bq-list(#"modifiers", getr(mods)),
                   bq-list(#"noun", getr(n)), getr(pp))));

defnode(pp, cat(prep, pp/prep, setr(prep, \*)));

defnode(pp/prep, down(np, pp/np, setr(op, \*)));

defnode(pp/np,
        up(bq-list(#"pp", bq-list(#"prep", getr(prep)),
                   bq-list(#"obj", getr(op)))));

defnode(s, down(np, s/subj, setr(mood, #"decl"), setr(subj, \*)),
        cat(v, v, setr(mood, #"imp"),
            setr(subj, #(#"np", #(#"pron", #"you"))), setr(aux, #f),
            setr(v, \*)));

defnode(s/subj, cat(v, v, setr(aux, #f), setr(v, \*)));

defnode(v,
        up(bq-list(#"s", bq-list(#"mood", getr(mood)),
                   bq-list(#"subj", getr(subj)),
                   bq-list(#"vcl", bq-list(#"aux", getr(aux)),
                           bq-list(#"v", getr(v))))),
        down(np, s/obj, setr(obj, \*)));

defnode(s/obj,
        up(bq-list(#"s", bq-list(#"mood", getr(mood)),
                   bq-list(#"subj", getr(subj)),
                   bq-list(#"vcl", bq-list(#"aux", getr(aux)),
                           bq-list(#"v", getr(v))),
                   bq-list(#"obj", getr(obj)))));

// LTD: No macros.
#"with-inference";

define method rep_ (x)
  if (not(instance?(x, <list>)))
    if (x == #"_") generate-symbol(#"string"("?")); else x; end if;
  else
    pair(rep_(head(x)), rep_(tail(x)));
  end if;
end method rep_;

define method fullbind (x, b)
  if (varsym?(x))
    aif2(binding(x, b), fullbind(it, b), generate-symbol());
  elseif (not(instance?(x, <list>)))
    x;
  else
    pair(fullbind(head(x), b), fullbind(tail(x), b));
  end if;
end method fullbind;

define method varsym? (x)
  instance?(x, <symbol>) & as(<string>, x)[0] == '?';
end method varsym?;

// LTD: No macros.
#"with-inference";

define method varsym? (x)
  instance?(x, <symbol>)
   & ~ // LTD: Function SYMBOL-PACKAGE not yet implemented.
       symbol-package(x);
end method varsym?;

define method gen-query (expr, #key binds)
  select (car(expr))
    #"and"
       => gen-and(tail(expr), binds);
    #"or"
       => gen-or(tail(expr), binds);
    #"not"
       => gen-not(second(expr), binds);
    otherwise
       => bq-list(#"prove",
                  bq-list*(#"list", bq-list(#"quote", head(expr)),
                           map(form, tail(expr))),
                  binds);
  end select;
end method gen-query;

define method gen-and (clauses, binds)
  if (empty?(clauses))
    bq-list(#"=values", binds);
  else
    let gb = generate-symbol();
    bq-list(#"=bind", bq-list(gb), gen-query(head(clauses), binds),
            gen-and(tail(clauses), gb));
  end if;
end method gen-and;

define method gen-or (clauses, binds)
  bq-cons(#"choose",
          map(method (c) gen-query(c, binds); end method, clauses));
end method gen-or;

define method gen-not (expr, binds)
  let gpaths = generate-symbol();
  bq-list(#"let", bq-list(bq-cons(gpaths, #(#"*paths*"))),
          #(#"setq", #"*paths*", #()),
          bq-list(#"choose",
                  bq-list*(#"=bind", #(#"b"), gen-query(expr, binds),
                           bq-list(#"setq", #"*paths*", gpaths),
                           #(#(#"fail"))),
                  bq-list(#"progn", bq-list(#"setq", #"*paths*", gpaths),
                          bq-list(#"=values", binds))));
end method gen-not;

=defun(prove, query(binds), choose-bind(r, *rules*, =funcall(r, query, binds)));

define method form (pat)
  if (simple?(pat))
    pat;
  else
    bq-list(#"cons", form(head(pat)), form(tail(pat)));
  end if;
end method form;

define variable *rules* = #f;

// LTD: No macros.
#"<-";

define method rule-fn (ant, con)
  with-gensyms(val(win, fact, binds),
               list(#"=lambda", list(fact, binds),
                    list(#"with-gensyms", vars-in(list(ant, con), simple?),
                         list(#"multiple-value-bind", list(val, win),
                              list(#"match", fact,
                                   apply(list,
                                         #"list",
                                         list(#"quote", head(con)),
                                         map(form, tail(con))),
                                   binds),
                              apply(list, #"if", win, gen-query(ant, val),
                                    #(#(#"fail")))))));
end method rule-fn;

define method rule-fn (ant, con)
  with-gensyms(val(win, fact, binds, paths), // 
               apply(list, #"=lambda", list(fact, binds, paths),
                     #(// 
                       #(#"with-gensyms",
                         #(#(#","), #"vars-in", #(#"list", #"ant", #"con"),
                           #(#"function", #"simple?")),
                         #(#"multiple-value-bind",
                           #(#(#(#",") . #"val"), #(#(#",") . #"win")),
                           #(#"match", #(#(#",") . #"fact"),
                             #(#"list",
                               #(#"quote", #(#(#","), #"car", #"con")),
                               #(#(#",@"), #"mapcar", #(#"function", #"form"),
                                 #(#"cdr", #"con"))),
                             #(#(#",") . #"binds")),
                           #(#"if", #(#(#",") . #"win"),
                             #(#(#","), #"gen-query", #"ant", #"val",
                               #"paths"),
                             // 
                             #(#"fail")))))));
end method rule-fn;

// LTD: No macros.
#"with-inference";

define method gen-query (expr, binds, paths)
  // 
  select (car(expr))
    #"and"
       => gen-and(tail(expr), binds, paths);
    #"or"
       => gen-or(tail(expr), binds, paths);
    #"not"
       => gen-not(second(expr), binds, paths);
    #"lisp"
       => gen-lisp(second(expr), binds);
    #"is"
       => gen-is(second(expr), third(expr), binds);
    #"cut"
       => apply(list, #"progn", list(#"setq", #"*paths*", paths),
                #(// 
                  #(#"=values", #(#(#",") . #"binds"))));
    #"t"
       => apply(list, #"prove",
                apply(list, #"list", list(#"quote", head(expr)),
                      map(form, tail(expr))),
                binds, #(#"*paths*"));
  end select;
end method gen-query;

// 
=defun(prove, query(binds, paths), // 
       choose-bind(r, *rules*, =funcall(r, query, binds, paths)));

// 
define method gen-and (clauses, binds, paths)
  // 
  if (empty?(clauses))
    list(#"=values", binds);
  else
    let gb = generate-symbol();
    apply(list, #"=bind", list(gb), gen-query(head(clauses), binds, paths),
          #(// 
            #(#(#","), #"gen-and", #(#"cdr", #"clauses"), #"gb", #"paths")));
  end if;
end method gen-and;

// 
define method gen-or (clauses, binds, paths)
  // 
  pair(#"choose",
       map(method (c) gen-query(c, binds, paths); end method, // 
           clauses));
end method gen-or;

define method gen-not (expr, binds, paths)
  let gpaths = generate-symbol();
  list(#"let", list(pair(gpaths, #(#"*paths*"))), #(#"setq", #"*paths*", #()),
       list(#"choose",
            apply(list, #"=bind", #(#"b"), gen-query(expr, binds, paths),
                  #(// 
                    #(#"setq", #"*paths*", #(#(#",") . #"gpaths")),
                    #(#"fail"))),
            list(#"progn", list(#"setq", #"*paths*", gpaths),
                 list(#"=values", binds))));
end method gen-not;

// LTD: No macros.
#"with-binds";

define method gen-lisp (expr, binds)
  apply(list, #"if", list(#"with-binds", binds, expr),
        list(#"=values", binds), #(#(#"fail")));
end method gen-lisp;

define method gen-is (expr1, expr2, binds)
  apply(list, #"aif2",
        list(#"match", expr1, list(#"with-binds", binds, expr2), binds),
        #(#(#"=values", #"it"), #(#"fail")));
end method gen-is;

define method rget (obj, prop)
  some2(method (a) a[prop]; end method, get-ancestors(obj));
end method rget;

define method get-ancestors (obj)
  local method getall (x)
          concatenate(list(x),
                      apply(concatenate!, map(getall, x[#"parents"])));
        end method getall;
  sort!(cl-remove-duplicates!(getall(obj)), stable: #t,
        test: method (x, y) member?(y, x[#"parents"]); end method);
end method get-ancestors;

define method some2 (fn, lst)
  if (not(instance?(lst, <list>)))
    #f;
  else
    let (val, win) = fn(head(lst));
    if (val | win) values(val, win); else some2(fn, tail(lst)); end if;
  end if;
end method some2;

define method obj (#rest parents)
  let obj = make(<table>);
  obj[#"parents"] := parents;
  ancestors(obj);
  obj;
end method obj;

define method ancestors (obj)
  obj[#"ancestors"] | (obj[#"ancestors"] := get-ancestors(obj));
end method ancestors;

define method rget (obj, prop)
  some2(method (a) a[prop]; end method, ancestors(obj));
end method rget;

// LTD: No macros.
#"defprop";

define method run-methods (obj, name, args)
  let meth = rget(obj, name);
  if (meth)
    apply(meth, obj, args);
  else
    error("No %S method for %S.", name, obj);
  end if;
end method run-methods;

define class <meth> (<object>)
  slot meth-around, init-keyword: #"meth-around";
  slot meth-before, init-keyword: #"meth-before";
  slot meth-primary, init-keyword: #"meth-primary";
  slot meth-after, init-keyword: #"meth-after";
end class <meth>;

// LTD: No macros.
#"meth-";

define method run-methods (obj, name, args)
  let pri = rget(obj, name, #"primary");
  if (pri)
    let ar = rget(obj, name, #"around");
    if (ar)
      apply(ar, obj, args);
    else
      run-core-methods(obj, name, args, pri);
    end if;
  else
    error("No primary %S method for %S.", name, obj);
  end if;
end method run-methods;

define method run-core-methods (obj, name, args, #key pri)
  let (#rest _)
      = begin
          run-befores(obj, name, args);
          apply(pri | rget(obj, name, #"primary"), obj, args);
        end;
  run-afters(obj, name, args);
  apply(values, _);
end method run-core-methods;

define method rget (obj, prop, #key meth, skip = 0)
  some2(method (a)
          let (val, win) = a[prop];
          if (win)
            select (meth)
              #"around"
                 => meth-(around, val);
              #"primary"
                 => meth-(primary, val);
              otherwise
                 => values(val, win);
            end select;
          end if;
        end method,
        nth-tail(ancestors(obj), skip));
end method rget;

define method run-befores (obj, prop, args)
  for (a in ancestors(obj))
    let bm = meth-(before, a[prop]);
    if (bm) apply(bm, obj, args); end if;
  end for;
end method run-befores;

define method run-afters (obj, prop, args)
  local method rec (lst)
          if (lst)
            rec(tail(lst));
            let am = meth-(after, head(lst)[prop]);
            if (am) apply(am, head(lst), args); end if;
          end if;
        end method rec;
  rec(ancestors(obj));
end method run-afters;

// LTD: No macros.
#"defmeth";

define method build-meth (name, type, gobj, parms, body)
  let gargs = generate-symbol();
  bq-list(#"function",
          bq-list(#"lambda", bq-list(#"&rest", gargs),
                  bq-list(#"labels",
                          bq-list(bq-list(#"call-next",
                                          #(),
                                          if (type == #"primary"
                                               | type == #"around")
                                          bq-list(#"cnm",
                                                  gobj,
                                                  bq-list(#"quote", name),
                                                  bq-list(#"cdr", gargs),
                                                  type);
                                          else
                                          #(#"error", "Illegal call-next.");
                                          end if),
                                  bq-list(#"next-p",
                                          #(),
                                          select (type)
                                          #"around"
                                           => bq-list(#"or",
                                                      bq-list*(#"rget",
                                                               gobj,
                                                               bq-list(#"quote",
                                                                       name),
                                                               #(#"around",
                                                                 1)),
                                                      bq-list*(#"rget",
                                                               gobj,
                                                               bq-list(#"quote",
                                                                       name),
                                                               #(#"primary")));
                                          #"primary"
                                           => bq-list*(#"rget",
                                                       gobj,
                                                       bq-list(#"quote",
                                                               name),
                                                       #(#"primary", 1));
                                          otherwise
                                           => #f;
                                          end select)),
                          bq-list(#"apply",
                                  bq-list(#"function",
                                          bq-list*(#"lambda", parms, body)),
                                  gargs))));
end method build-meth;

define method cnm (obj, name, args, type)
  select (type)
    #"around"
       => let ar = rget(obj, name, around: 1);
           if (ar)
             apply(ar, obj, args);
           else
             run-core-methods(obj, name, args);
           end if;
    #"primary"
       => let pri = rget(obj, name, primary: 1);
           if (pri)
             apply(pri, obj, args);
           else
             error("No next method.");
           end if;
    otherwise
       => #f;
  end select;
end method cnm;

// LTD: No macros.
#"undefmeth";

// LTD: No macros.
#"children";

define method parents (obj) obj[#"parents"]; end method parents;

define method set-parents (obj, pars)
  for (p in parents(obj)) children(p) := remove!(children(p), obj); end for;
  obj[#"parents"] := pars;
  for (p in pars)
    let g15940 = obj;
    let g15939 = p;
    let g15938 = add!(g15940, children(g15939));
    children-setter(g15938, g15939);
  end for;
  maphier(method (obj) obj[#"ancestors"] := get-ancestors(obj); end method,
          obj);
  pars;
end method set-parents;

// LTD: No setf macros.
#"parents";

define method maphier (fn, obj)
  fn(obj);
  for (c in children(obj)) maphier(fn, c); end for;
end method maphier;

define method obj (#rest parents)
  let obj = make(<table>);
  parents(obj) := parents;
  obj;
end method obj;

// LTD: No macros.
#"defcomb";

define method run-core-methods (obj, name, args, #key pri)
  let comb = symbol-get-property(name, #"mcombine");
  if (comb)
    if (instance?(comb, <symbol>))
      (select (comb)
         #"and"
            => comb-and;
         #"or"
            => comb-or;
         otherwise
            => #f;
       end select)(obj, name, args, ancestors(obj));
    else
      comb-normal(comb, obj, name, args);
    end if;
  else
    let (#rest _)
        = begin
            run-befores(obj, name, args);
            apply(pri | rget(obj, name, #"primary"), obj, args);
          end;
    run-afters(obj, name, args);
    apply(values, _);
  end if;
end method run-core-methods;

define method comb-normal (comb, obj, name, args)
  apply(comb,
        apply(concatenate!,
              map(method (a)
                    let pm = meth-(primary, a[name]);
                    let val = if (pm) apply(pm, obj, args); end if;
                    if (val) list(val); end if;
                  end method,
                  ancestors(obj))));
end method comb-normal;

define method comb-and (obj, name, args, ancs, #key last = #t)
  if (empty?(ancs))
    last;
  else
    let pm = meth-(primary, head(ancs)[name]);
    if (pm)
      let new = apply(pm, obj, args);
      new & comb-and(obj, name, args, tail(ancs), new);
    else
      comb-and(obj, name, args, tail(ancs), last);
    end if;
  end if;
end method comb-and;

define method comb-or (obj, name, args, ancs)
  ancs
   & begin
       let pm = meth-(primary, head(ancs)[name]);
       (pm & apply(pm, obj, args) | comb-or(obj, name, args, tail(ancs)));
     end;
end method comb-or;

// LTD: No macros.
#"undefmethod";

define method udm (name, qual, specs)
  let classes
      = map(method (s)
              bq-list(#"find-class", bq-list(#"quote", s));
            end method,
            specs);
  bq-list(#"remove-method",
          bq-list(#"symbol-function", bq-list(#"quote", name)),
          bq-list(#"find-method",
                  bq-list(#"symbol-function", bq-list(#"quote", name)),
                  bq-list(#"quote", qual), bq-cons(#"list", classes)));
end method udm;

define method compall ()
  let g15941 = package-name-cvt-1(*package*);
  block (return)
    begin
      begin
        let g15943
            = method (s, #key .do-symbols-dummy-val.)
                if (// LTD: Function FBOUNDP not yet implemented.
                    fboundp(s))
                  if (~ instance?(s, <function>))
                    print(s, *standard-output*);
                    // LTD: Function COMPILE not yet implemented.
                    compile(s);
                  end if;
                end if;
              end method;
        let syms = package-internal-symbols(g15941);
        if (syms) do(g15943, key-sequence(syms), syms); end if;
        let tab15015 = package-external-symbols(g15941);
        do(g15943, key-sequence(tab15015), tab15015);
        for (g15942 in // LTD: Function PACKAGE-USE-LIST not yet implemented.
                       package-use-list(g15941))
          let tab15015 = package-external-symbols(g15942);
          do(method (.do-symbols-dummy-symbolx., .do-symbols-dummy-val.)
               if (~ member?(.do-symbols-dummy-symbolx.,
                             package-internal-shadowing-symbols(g15941),
                             test: method (x, y)
                                     (x
                                       = (method (s)
                                          as(<string>, s);
                                          end method)(y));
                                   end method))
                 g15943(.do-symbols-dummy-symbolx.);
               end if;
             end method,
             key-sequence(tab15015), tab15015);
        end for;
      end;
      return(#f);
    end;
  end block;
end method compall;

// LTD: No macros.
#"check";

//  This code is copyright 1993 by Paul Graham, but anyone who wants 
//  to use the code in any nonprofit activity, or distribute free
//  verbatim copies (including this notice), is encouraged to do so.
"eof";

