//  -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
//  Copyright 1992 Patrick H. Winston.  All rights reserved.
//  Version 1.1.1, copied from master file on 23 Apr 93       
//  
//  This software is licensed by Patrick H. Winston (licensor) for
//  instructional use with the textbooks ``Artificial Intelligence,'' by
//  Patrick H. Winston, and ``Lisp,'' by Patrick H. Winston and Berthold
//  K. P. Horn.  Your are free to make copies of this software and
//  modify it for such instructional use as long as:
//  1. You keep this notice intact.
//  2. You cause any modified files to carry a prominent notice stating
//     that you modified the files and the date of your modifications.
//  This software is licensed ``AS IS'' without warranty and the licensor
//  shall have no liability for any alleged defect or damages.
// 
// 
// The procedures in this fill offer a small subset of the functionality
// of the Common Lisp Object System (CLOS).
// 
// Note that the emphasis is on simplicity, not speed.  Accordingly,
// values are kept on property lists, and methods are kept on
// association lists.
// 
// Only simple versions of DEFCLASS, MAKE-INSTANCE, and DEFMETHOD
// are provided; :around :before, :primary, and :after methods are
// provided.
// 
// Methods can be specialized to instances using the following in
// the position normally occupied in a DEFMETHOD form by a class name:
// (EQL <form evaluating to instance>).
// 
// SETF methods are allowed using the following in the position
// normally occupied in a DEFMETHOD form by a method name:
// (setf <method name>)
// 
// The procedures are meant to constitute an abstraction layer.
// Hence no effort hs been made to comment them properly.
// 
// 
//   DYNAMIC VARIABLES
define variable *around-methods* = #f;

define variable *before-methods* = #f;

define variable *primary-methods* = #f;

define variable *after-methods* = #f;

define variable *args* = #f;

//  METHOD DEFINITION AND INTERPRETATION
// LTD: No macros.
#"defmethod";

define method process-methods (name, args)
  fluid-bind (*around-methods* = fetch-methods(name, args, #"around"))
    fluid-bind (*before-methods* = fetch-methods(name, args, #"before"))
      fluid-bind (*primary-methods* = fetch-methods(name, args, #"primary"))
        fluid-bind (*after-methods* = fetch-methods(name, args, #"after"))
          next-method();
        end fluid-bind;
      end fluid-bind;
    end fluid-bind;
  end fluid-bind;
end method process-methods;

define method call-next-method ()
  if (*around-methods*)
    apply(pop!(*around-methods*), *args*);
  else
    for (until not(pair?(*before-methods*)))
      apply(pop!(*before-methods*), *args*);
    end for;
    let (#rest _)
        = if (*primary-methods*)
            apply(pop!(*primary-methods*), *args*);
          else
            error("Oops, no applicable primary method!");
          end if;
    for (until not(pair?(*after-methods*)))
      apply(pop!(*after-methods*), *args*);
    end for;
    apply(values, _);
  end if;
end method call-next-method;

define method add-method (name, key, function, type)
  let methods = symbol-get-property(name, type);
  let current = cl-assoc(key, methods, test: \=);
  if (methods)
    if (~ (size(key) = size(first(first(methods)))))
      error("Ooops new %S method has inconsistent number of arguments.",
            name);
    end if;
  end if;
  if (current)
    second(current) := function;
    name;
  else
    push!(list(key, function), symbol-get-property(name, type));
  end if;
end method add-method;

//  METHOD REMOVAL
define method remove-methods (name)
  symbol-get-property(name, #"primary") := #f;
  symbol-get-property(name, #"before") := #f;
  symbol-get-property(name, #"after") := #f;
  symbol-get-property(name, #"around") := #f;
end method remove-methods;

//  METHOD FILTERING AND ARRANGING
define method fetch-methods (name, args, type)
  let precedence-lists
      = map(method (arg, class)
              // Allow specialization to instances:
              pair(arg,
                   symbol-get-property(class, #"precedence-list")
                    | make-precedence-list(class));
            end method,
            args, map(get-argument-class, args));
  let methods = symbol-get-property(name, type);
  methods
   := remove(methods,
             complement(method (pair)
                          applicable-p(first(pair), precedence-lists);
                        end method));
  methods
   := sort!(methods,
            test: method (x, y)
                    higher-p(first(x), first(y), precedence-lists);
                  end method);
  map(second, methods);
end method fetch-methods;

define method higher-p (m1, m2, precedence-lists)
  if (m1 = m2)
    #f;
  else
    let n1 = find-key(first(precedence-lists), curry(\==, first(m1)));
    let n2 = find-key(first(precedence-lists), curry(\==, first(m2)));
    if (n1 = n2)
      higher-p(tail(m1), tail(m2), tail(precedence-lists));
    else
      n1 < n2;
    end if;
  end if;
end method higher-p;

define method applicable-p (specializers, lists)
  block (return)
    for (specializers = specializers then rest(specializers),
         lists = lists then rest(lists), until not(pair?(specializers)))
      // Allow specialization to instances:
      if (~ if (instance?(first(specializers), <list>))
              (second(first(specializers)) == first(first(lists)));
            else
              member?(first(specializers), first(lists));
            end if)
        return(#f);
      end if;
    finally
      #t;
    end for;
  end block;
end method applicable-p;

define method get-argument-class (arg)
  if (instance?(arg, <symbol>) & symbol-get-property(arg, #"is-a"))
    symbol-get-property(arg, #"is-a");
  else
    let result = object-class(arg);
    if (instance?(result, <list>)) first(result); else result; end if;
  end if;
end method get-argument-class;

//  PRECEDENCE LIST CONSTRUCTION
define method make-precedence-list (object)
  let superclasses = make-relatives-list(object, #"superclasses");
  establish-order(superclasses,
                  map(method (class)
                        pair(class,
                             symbol-get-property(class, #"superclasses")
                              | #(#"t"));
                      end method,
                      superclasses));
end method make-precedence-list;

define method make-relatives-list (class, property)
  for (result = classes then classes, until empty?(classes))
    let probes = symbol-get-property(pop!(classes), property);
    for (probe in probes)
      if (probe == class)
        error("Beware! %S is circular!", class);
      else
        result := add!(probe, result);
        classes := add!(probe, classes);
      end if;
    end for;
  finally
    result;
  end for;
end method make-relatives-list;

define method establish-order (classes, direct-supers)
  let precedence-list = #f;
  let pairs = construct-pairs(direct-supers);
  block (return)
    while (#t)
      if (not(pair?(classes)))
        return(reverse(pair(#t, precedence-list)));
      end if;
      let candidates = filter-classes(classes, pairs);
      let winner
          = filter-candidates(candidates, precedence-list, direct-supers);
      // Shrink the list of precedence pairs:
      pairs := filter-pairs(pairs, winner);
      // Move the winning class to the precedence list:
      classes := remove(classes, winner);
      push!(winner, precedence-list);
    end while;
  end block;
end method establish-order;

define method construct-pairs (direct-supers)
  apply(concatenate, map(construct-pairs-aux, direct-supers));
end method construct-pairs;

define method construct-pairs-aux (l)
  if (not(pair?(tail(tail(l)))))
    list(l);
  else
    pair(list(first(l), second(l)), construct-pairs-aux(tail(l)));
  end if;
end method construct-pairs-aux;

define method filter-classes (classes, precedence-pairs)
  for (class in classes)
    if (~ member?(class, precedence-pairs,
                  test: method (x, y) (x == second(y)); end method))
      push!(class, result);
    end if;
  end for;
  select (length(result))
    0
       => error("Precedence list for \n%S\ncannot be computed.",
                precedence-pairs);
    otherwise
       => result;
  end select;
end method filter-classes;

define method filter-candidates (candidates, precedence-list, direct-supers)
  select (length(candidates))
    1
       => first(candidates);
    otherwise
       => block (found-it)
            for (possible-subclass in precedence-list)
              for (candidate in candidates)
                if (member?(candidate,
                            cl-assoc(possible-subclass, direct-supers)))
                  found-it(candidate);
                end if;
              end for;
            end for;
          end block;
  end select;
end method filter-candidates;

define method filter-pairs (precedence-pairs, winner)
  choose(complement(method (pair) winner == first(pair); end method),
         precedence-pairs);
end method filter-pairs;

//  CLASS DEFINITION
// LTD: No macros.
#"defclass";

define method process-initargs (object, slots, initargs)
  for (slot in slots)
    if (~ member?(first(slot), symbol-plist(object)))
      let apointer = member?(initarg: slot);
      if (apointer & member?(second(apointer), initargs))
        symbol-get-property(object, first(slot))
         := second(member?(second(apointer), initargs));
      end if;
    end if;
  end for;
end method process-initargs;

define method process-initforms (object, slots)
  for (slot in slots)
    if (~ member?(first(slot), symbol-plist(object)))
      let fpointer = member?(initform: slot);
      if (fpointer)
        symbol-get-property(object, first(slot))
         := // LTD: Function EVAL not yet implemented.
            eval(second(fpointer));
      end if;
    end if;
  end for;
end method process-initforms;

define method process-precedence (class, superclasses)
  symbol-get-property(class, #"superclasses") := superclasses;
  for (s in superclasses)
    let new-value-146963 = class;
    let g146959 = s;
    let g146960 = #"downlink";
    let g146962
        = add!(new-value-146963, symbol-get-property(g146959, g146960));
    %put(g146959, g146960, g146962);
  end for;
  for (s in make-relatives-list(class, #"downlink"))
    symbol-get-property(s, #"precedence-list") := make-precedence-list(s);
  end for;
end method process-precedence;

define method process-accessor (class, slot-description)
  let slot = first(slot-description);
  let reader = second(member?(accessor: slot-description));
  let writer = make-setf-symbol(reader);
  if (reader)
    // LTD: Function EVAL not yet implemented.
    eval(list(#"defmethod", reader, list(list(#"object", class)),
              list(#"get", #"object", list(#"quote", slot))));
    // LTD: Function EVAL not yet implemented.
    eval(list(#"defmethod", writer, pair(list(#"object", class), #(#"value")),
              apply(list, #"setf",
                    list(#"get", #"object", list(#"quote", slot)),
                    #(#"value"))));
    // LTD: Function EVAL not yet implemented.
    eval(list(#"defsetf", reader, writer));
  end if;
end method process-accessor;

define method make-setf-symbol (symbol)
  as(<symbol>, concatenate-as(<string>, "SETF-", as(<string>, symbol)));
end method make-setf-symbol;

//  INSTANCE DEFINITION
define method make-instance (class, #rest args)
  let instance = generate-symbol(#"string"(format(#f, "%S-", class)));
  symbol-get-property(instance, #"is-a") := class;
  initialize-initargs(instance, args);
  initialize-initforms(instance);
end method make-instance;

define method initialize-initargs (object :: <object>, initargs :: <object>)
  object;
end method initialize-initargs;

define method initialize-initforms (object :: <object>)
  object;
end method initialize-initforms;

//  MISCELLANEOUS ACCESSORS
define method class-of (x) symbol-get-property(x, #"is-a"); end method class-of;

define method precedence-list-of (x)
  symbol-get-property(x, #"precedence-list");
end method precedence-list-of;

//  INSTANCE AND CLASS DISPLAY
define method show (object)
  local method find-width (symbol)
          for (l = symbol-plist(symbol) then rest(rest(l)),
               until not(pair?(l)))
            let w = size(format(#f, "%S", first(l)));
            if (w > result) result := w; end if;
          finally
            result;
          end for;
        end method find-width,
        method show-properties (symbol)
          let width = find-width(symbol);
          for (l = symbol-plist(symbol) then rest(rest(l)),
               until not(pair?(l)))
            if (~ (#"is-a" == first(l)))
              format-out("\n  %S:%S  %S", first(l),
                         make(<string>,
                              size: width - size(format(#f, "%S", first(l))),
                              fill: ' '),
                         second(l));
            end if;
          end for;
        end method show-properties,
        method instance-p (symbol)
          symbol-get-property(symbol, #"is-a");
        end method instance-p,
        method class-p (symbol)
          symbol-get-property(symbol, #"superclasses");
        end method class-p;
  if (instance-p(object))
    format-out("\nInstance %S:", object);
    show-properties(object);
    show(symbol-get-property(object, #"is-a"));
  elseif (class-p(object))
    format-out("\nClass %S:\n", object);
    for (l in tail(precedence-list-of(object)))
      format-out("  %S", l);
    end for;
  else
    format-out("\nNot an instance or class.");
  end if;
  values();
end method show;

