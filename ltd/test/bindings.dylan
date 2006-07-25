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
  check-lock-definitions-compile-time(#"popf", #"function", #"defmacro",
                                      // LTD: Function FBOUNDP not yet implemented.
                                      fboundp(#"popf"));
  // LTD: Function MACRO-FUNCTION not yet implemented.
  macro-function(#"popf")
   := method (**macroarg**, ..environment..)
        dt-macro-argument-check(2, #f, **macroarg**, #"macro");
        let env = ..environment..;
        let g15957 = tail(**macroarg**);
        let %reference = car-fussy(g15957, #"%reference");
        let item = car-fussy(tail(g15957), #"item");
        let keywords = tail(tail(g15957));
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
                                                 #"pop-fn",
                                                 getter,
                                                 item,
                                                 keywords));
                                    else
                                      apply(list,
                                            #"pop-fn",
                                            getter,
                                            item,
                                            keywords);
                                    end if),
                  setter);
        end for;
      end method;
  set-func_name(// LTD: Function MACRO-FUNCTION not yet implemented.
                macro-function(#"popf"),
                #"popf");
  .inv-func_formals(// LTD: Function FBOUNDP not yet implemented.
                    fboundp(#"popf"),
                    #(#"%reference", #"item", #"&rest", #"keywords"));
  ce-putprop(#"popf",
             method (**macroarg**, ..environment..)
               dt-macro-argument-check(2, #f, **macroarg**, #"macro");
               let env = ..environment..;
               let g15957 = tail(**macroarg**);
               let %reference = car-fussy(g15957, #"%reference");
               let item = car-fussy(tail(g15957), #"item");
               let keywords = tail(tail(g15957));
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
                                                      #"pop-fn",
                                                      getter,
                                                      item,
                                                      keywords));
                                           else
                                           apply(list,
                                                 #"pop-fn",
                                                 getter,
                                                 item,
                                                 keywords);
                                           end if),
                         setter);
               end for;
             end method,
             #".compile-file-macro.");
  symbol-remove-property(#"popf", #"%fun-documentation");
  record-source-file(#"popf");
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

