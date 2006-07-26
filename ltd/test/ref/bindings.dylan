//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
//  This file was stolen from Matt Ginsberg's MVL 8/7/92
"(in-package dtp)";

define module dtp export plug; end module dtp;

#f;

define method binding-variable (binding)
  head(binding);
end method binding-variable;

define method binding-value (binding) tail(binding); end method binding-value;

// ----------------------------------------------------------------------------
#"eval-when"(#"compile"(load: #"eval"),
             #"proclaim"(#(#"declaration", #"dynamic-extent")));

//  This file contains a variety of functions that manipulate binding
//  lists and "answers" - structures containing a binding list and a
//  truth value.
// 	Commented out by Don Geddis 5/12/93
// 
// (defvar true)
// 
// (defstruct (answer (:constructor make-answer (&optional binding value))
// 	    (:print-function print-answer))
//   (binding nil)
//   (value true))
// 
// (defun print-answer (answer stream print-depth)
//   (declare (ignore print-depth))
//   (format stream #+ANSI "~@<#S(~1IANSWER~:_ :BINDING~:_ ~s~:_ :VALUE~:_ ~s)~:>"
// 	         #-ANSI "#S(ANSWER :BINDING ~s :VALUE ~s)"
// 	  (answer-binding answer)
// 	  (mvl-print (answer-value answer))))
// 
// ;; a few random utilities.
// ;;   invert-answer takes an answer and negates the truth value
// ;;   equal-answer checks two answers for equality
// 
// (defun invert-answer (answer)
//   (make-answer (answer-binding answer) (mvl-not (answer-value answer))))
// 
// (defun equal-answer (a1 a2)
//   (and (typep a1 'answer) (typep a2 'answer)
//        (equal-binding (answer-binding a1) (answer-binding a2))
//        (mvl-eq (answer-value a1) (answer-value a2))))
// 
// 
//  popf				same keywords as delete
//  plug (x bdg-list)		plug binding list into x
//  delete-bdg (x bdg-list)	remove binding for x in bdg-list
//  get-bdg (x bdg-list)		find binding for x in bdg-list
begin
  fluid-bind (*function-name*
               = generate-subform-name(#"popf", *function-name*))
    fluid-bind (*function-parent* = tlf-function-parent(#(#"quote", #"popf")))
      record-sf-eval(compiler-eval(*function-name*),
                     compiler-eval(*function-parent*));
      record-sf-compile(*function-name*, *function-parent*);
      set-macro-function(#"popf",
                         method (%%macroarg%%, environment)
                           let &whole151751 = %%macroarg%%;
                           let (%reference ...)151752 = tail(&whole151751);
                           let check-lambda-list-top-level151755
                               = check-lambda-list-top-level(#(#"%reference",
                                                               #"item",
                                                               #"&rest",
                                                               #"keywords"),
                                                             &whole151751,
                                                             (%reference ...)151752,
                                                             2,
                                                             2,
                                                             #"t",
                                                             #"macro");
                           let %reference = head((%reference ...)151752);
                           let (item ...)151753
                               = tail((%reference ...)151752);
                           let item = head((item ...)151753);
                           let keywords151754 = tail((item ...)151753);
                           let keywords = keywords151754;
                           begin
                             #f;
                             let (dummies, vals, newval, setter, getter)
                                 = // LTD: Function GET-SETF-METHOD not yet implemented.
                                   get-setf-method(%reference, environment);
                             for (d = dummies then cdr(d),
                                  v = vals then cdr(v),
                                  let-list = nil then cons(list(car(d),
                                                                car(v)),
                                                           let-list),
                                  until empty?(d))
                               #f;
                             finally
                               values(push!(list(head(newval),
                                                 apply(list,
                                                       #"pop-fn",
                                                       getter,
                                                       item,
                                                       keywords)),
                                            let-list),
                                      list(#"let*",
                                           reverse!(let-list),
                                           setter));
                             end for;
                           end;
                         end method);
      broadcast-redefined(#"popf",
                          macro: #(#(#"let*",
                                     #(#(#"&whole151751", #"%%macroarg%%"),
                                       #(#"(%reference ...)151752",
                                         #(#"cdr", #"&whole151751")),
                                       #(#"check-lambda-list-top-level151755",
                                         #(#"check-lambda-list-top-level",
                                           #(#"quote",
                                             #(#"%reference",
                                               #"item",
                                               #"&rest",
                                               #"keywords")),
                                           #"&whole151751",
                                           #"(%reference ...)151752",
                                           2,
                                           2,
                                           #(#"quote", #"t"),
                                           #"macro")),
                                       #(#"%reference",
                                         #(#"car",
                                           #(#"the-cons",
                                             #"(%reference ...)151752"))),
                                       #(#"(item ...)151753",
                                         #(#"cdr",
                                           #(#"the-cons",
                                             #"(%reference ...)151752"))),
                                       #(#"item",
                                         #(#"car",
                                           #(#"the-cons",
                                             #"(item ...)151753"))),
                                       #(#"keywords151754",
                                         #(#"cdr",
                                           #(#"the-cons",
                                             #"(item ...)151753"))),
                                       #(#"keywords", #"keywords151754")),
                                     #(#"block",
                                       #"popf",
                                       #(),
                                       #(#"multiple-value-bind",
                                         #(#"dummies",
                                           #"vals",
                                           #"newval",
                                           #"setter",
                                           #"getter"),
                                         #(#"get-setf-method",
                                           #"%reference",
                                           #"environment"),
                                         #(#"do",
                                           #(#(#"d",
                                               #"dummies",
                                               #(#"cdr", #"d")),
                                             #(#"v",
                                               #"vals",
                                               #(#"cdr", #"v")),
                                             #(#"let-list",
                                               #(),
                                               #(#"cons",
                                                 #(#"list",
                                                   #(#"car", #"d"),
                                                   #(#"car", #"v")),
                                                 #"let-list"))),
                                           #(#(#"null", #"d"),
                                             #(#"push",
                                               #(#"list",
                                                 #(#"car", #"newval"),
                                                 #(#"list*",
                                                   #(#"quote", #"pop-fn"),
                                                   #"getter",
                                                   #"item",
                                                   #"keywords")),
                                               #"let-list"),
                                             #(#"list",
                                               #(#"quote", #"let*"),
                                               #(#"nreverse", #"let-list"),
                                               #"setter"))))))));
      symbol-remove-property(#"popf", #"%fun-documentation");
      flag-symbol-macro$symbol(#"popf");
    end fluid-bind;
  end fluid-bind;
  #"popf";
end;

define method pop-fn (list, item, #rest key)
  apply(cl-remove!, item, list, key);
end method pop-fn;

//  plug.  If the binding list is empty, then you can just return x.  If
//  the binding list is not empty, then walk down the expression x; if x
//  is a variable on the binding list, do the substitution.  If x is an
//  atom not on the binding list, then just return it.  If x is a cons,
//  keep walking.  But as you walk, you have to be careful to handle
//  sequence variables correctly.  So if the car of x is a simple
//  variable, then you replace it with its value and continue.  If
//  the car of x is a sequence variable, do an append.
define method plug (x, bdg-list)
  if (bdg-list) plug1(x, bdg-list); else x; end if;
end method plug;

define method plug1 (x, bdg-list)
  if (varp(x) & (temp := cl-assoc(x, bdg-list)))
    tail(temp);
  elseif (not(instance?(x, <list>)))
    x;
  elseif (varp*(head(x)))
    if (temp := cl-assoc(head(x), bdg-list))
      concatenate(tail(temp), plug1(tail(x), bdg-list));
    else
      reuse-cons(head(x), plug1(tail(x), bdg-list), x);
    end if;
  else
    reuse-cons(plug1(head(x), bdg-list), plug1(tail(x), bdg-list), x);
  end if;
end method plug1;

define method reuse-cons (car, cdr, orig)
  if (car == head(orig) & cdr == tail(orig))
    orig;
  else
    pair(car, cdr);
  end if;
end method reuse-cons;

//  The following functions delete the binding for a given variable or
//  variables from a binding list.
define method delete-bdg (x, bdg-list)
  remove!(bdg-list, x, test: method (x, y) x == head(y); end method,
          count: 1);
end method delete-bdg;

define method remove-bdg (x, bdg-list)
  remove(bdg-list, x, test: method (x, y) x == head(y); end method, count: 1);
end method remove-bdg;

define method delete-bdgs (vars, bdg-list)
  choose(complement(compose(method (x) member?(x, vars); end method, head)),
         bdg-list);
end method delete-bdgs;

define method remove-bdgs (vars, bdg-list)
  choose(complement(compose(method (x) member?(x, vars); end method, head)),
         bdg-list);
end method remove-bdgs;

//  Given a binding list and a list of variables, delete the bindings
//  for variables not on the list (which were spawned by intermediate
//  goals, presumably).
define method meaningful-bdgs (bdgs, vars)
  remove(bdgs, complement(method (x) member?(x, vars); end method),
         test: method (x, y) x == head(y); end method);
end method meaningful-bdgs;

//  Get the binding for a given variable from a binding list.  Note that
//  this piece of code (and others!) makes implicit use of the fact that
//  binding lists are never "nested", so that the binding list binding x
//  to y and then y to z will never appear in the system -- x will always
//  be bound to z directly.
define method get-bdg (x, bdg-list)
  tail(cl-assoc(x, bdg-list));
end method get-bdg;

