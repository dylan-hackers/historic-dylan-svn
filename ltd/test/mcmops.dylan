//  *********************************************************************
//  Copyright (c) 1989, Lawrence Erlbaum Assoc. All rights reserved.
//  
//  Use and copying of this software and preparation of derivative works
//  based upon this software are permitted.  Any distribution of this
//  software or derivative works must comply with all applicable United
//  States export control laws.
//  
//  This software is made available AS IS. The author makes no warranty
//  about the software, its performance or its conformity to any
//  specification.
//  *********************************************************************
// LTD: No macros.
#"insist";

define method make-insist-forms (fnname, exps)
  ~ empty?(exps)
   & pair(bq-list(#"or", head(exps),
                  bq-list(#"error", "~S failed in ~S",
                          bq-list(#"quote", head(exps)),
                          bq-list(#"quote", fnname))),
          make-insist-forms(fnname, tail(exps)));
end method make-insist-forms;

// LTD: No macros.
#"define-table";

define method delete-key (table, key)
  remove-property!(table, key);
  table;
end method delete-key;

define method table-keys (table)
  table & pair(head(table), table-keys(tail(tail(table))));
end method table-keys;

*for-keys* := #f;

define-table(for-key, key(), *for-keys*);

// LTD: No macros.
#"for";

define method for-var-forms (l)
  l & instance?(head(l), <list>) & pair(head(l), for-var-forms(tail(l)));
end method for-var-forms;

define method for-body (l)
  l & (for-key(head(l)) & l | for-body(tail(l)));
end method for-body;

define method for-expander (var-forms, when-form, body-forms)
  insist(for, ~ empty?(var-forms), ~ empty?(body-forms));
  let vars = map(head, var-forms);
  let lists
      = map(method (var-form) head(tail(tail(var-form))); end method,
            var-forms);
  let mapfn-body
      = (for-key(head(body-forms)))(when-form,
                                    bq-cons(#"progn", tail(body-forms)));
  bq-list*(head(mapfn-body),
           bq-list(#"function",
                   bq-list(#"lambda", vars, head(tail(mapfn-body)))),
           lists);
end method for-expander;

// LTD: No macros.
#"define-for-key";

define-for-key(always: test(body), #"every",
               if (test)
                 bq-list(#"or", bq-list(#"not", test), body);
               else
                 body;
               end if);

define-for-key(do: test(body), #"mapc",
               if (test) bq-list(#"and", test, body); else body; end if);

define-for-key(filter: test(body), #"mapcan",
               begin
                 let fbody
                     = bq-list*(#"let", bq-list(bq-list(#"x", body)),
                                #(#(#"and", #"x", #(#"list", #"x"))));
                 if (test) bq-list(#"and", test, fbody); else fbody; end if;
               end);

define-for-key(first: test(body), #"some",
               if (test) bq-list(#"and", test, body); else body; end if);

define-for-key(save: test(body), if (test) #"mapcan"; else #"mapcar"; end if,
               if (test)
                 bq-list(#"and", test, bq-list(#"list", body));
               else
                 body;
               end if);

define-for-key(splice: test(body), #"mapcan",
               bq-list(#"copy-list",
                       if (test)
                         bq-list(#"and", test, body);
                       else
                         body;
                       end if));

*mop-tables* := #f;

define-table(mop-table, table-name(), *mop-tables*);

define-table(mop-absts, mop(), mop-table(#"absts"));

define-table(mop-all-absts, mop(), mop-table(#"all-absts"));

define-table(mop-specs, mop(), mop-table(#"specs"));

define-table(mop-slots, mop(), mop-table(#"slots"));

define-table(mop-type, mop(), mop-table(#"type"));

define method mopp (x)
  instance?(x, <number>) | (instance?(x, <symbol>) & mop-type(x));
end method mopp;

define method instance-mopp (x)
  mopp(x) & (instance?(x, <number>) | mop-type(x) == #"instance");
end method instance-mopp;

define method abst-mopp (x)
  mopp(x) & mop-type(x) == #"mop";
end method abst-mopp;

define method abstp (abst, spec)
  abst == spec | member?(abst, mop-all-absts(spec));
end method abstp;

define method patternp (x) abstp(#"m-pattern", x); end method patternp;

define method groupp (x) abstp(#"m-group", x); end method groupp;

define method slot-role (slot) head(slot); end method slot-role;

define method slot-filler (slot) head(tail(slot)); end method slot-filler;

define method make-slot (role, mop) list(role, mop); end method make-slot;

define method role-slot (role, x)
  insist(role-slot, mopp(x) | instance?(x, <list>));
  cl-assoc(role, if (mopp(x)) mop-slots(x); else x; end if);
end method role-slot;

define method role-filler (role, x)
  slot-filler(role-slot(role, x));
end method role-filler;

define method add-role-filler (role, mop, filler)
  insist(add-role-filler, mopp(mop), empty?(role-filler(role, mop)));
  format-out("\n%=:%= <= %=", mop, role, filler);
  mop-slots(mop) := pair(make-slot(role, filler), mop-slots(mop));
  filler;
end method add-role-filler;

define method link-abst (spec, abst)
  insist(link-abst, abst-mopp(abst), mopp(spec), ~ abstp(spec, abst));
  if (~ abstp(abst, spec))
    mop-absts(spec) := pair(abst, mop-absts(spec));
    mop-specs(abst) := pair(spec, mop-specs(abst));
    redo-all-absts(spec);
  elseif (nil);
  end if;
  spec;
end method link-abst;

define method unlink-abst (spec, abst)
  if (abstp(abst, spec))
    mop-absts(spec) := remove(mop-absts(spec), abst);
    mop-specs(abst) := remove(mop-specs(abst), spec);
    redo-all-absts(spec);
  elseif (nil);
  end if;
  spec;
end method unlink-abst;

define method redo-all-absts (mop)
  mop-all-absts(mop) := calc-all-absts(mop);
  for (spec(in: mop-specs(mop))) #"do"; redo-all-absts(spec); end for;
end method redo-all-absts;

define method calc-all-absts (mop)
  cl-remove-duplicates(pair(mop,
                            for (abst(in: mop-absts(mop)))
                              #"splice";
                              mop-all-absts(abst);
                            end for));
end method calc-all-absts;

define method new-mop (name, absts, type, slots)
  insist(new-mop, instance?(name, <symbol>),
         for (abst(in: absts)) #"always"; mopp(abst); end for);
  type | (type := calc-type(absts, slots));
  name | (name := spec-name(absts, type));
  mop-type(name) := type;
  slots & (mop-slots(name) := slots);
  for (abst(in: absts)) #"do"; link-abst(name, abst); end for;
  name;
end method new-mop;

define method calc-type (absts, slots)
  for (abst(in: absts)) #"when"; patternp(abst); #"first"; #"mop"; end for
   | (empty?(slots) & #"mop")
   | for (slot(in: slots))
       #"when";
       ~ instance-mopp(slot-filler(slot));
       #"first";
       #"mop";
     end for
   | #"instance";
end method calc-type;

define method spec-name (absts, type)
  generate-symbol(#"string"((if (type == #"mop")
                               "~S.";
                             else
                               "I-~S.";
                             end if)(#f, head(absts))));
end method spec-name;

define method clear-memory ()
  *mop-tables* := #f;
  new-mop(#"m-root", #f, #"mop", #f);
  mop-all-absts(#"m-root") := calc-all-absts(#"m-root");
  #"m-root";
end method clear-memory;

define method all-mops () table-keys(mop-table(#"type")); end method all-mops;

define method remove-mop (name)
  for (abst(in: mop-absts(name))) #"do"; unlink-abst(name, abst); end for;
  for (table-name(in: table-keys(*mop-tables*)))
    #"do";
    mop-table(table-name) := delete-key(mop-table(table-name), name);
  end for;
end method remove-mop;

define method inherit-filler (role, mop)
  for (abst(in: mop-all-absts(mop)))
    #"first";
    role-filler(role, abst);
  end for;
end method inherit-filler;

define method get-filler (role, mop)
  role-filler(role, mop)
   | begin
       let filler = inherit-filler(role, mop);
       (filler
         & (instance-mopp(filler) & filler
             | (abstp(#"m-function", filler) & filler)
             | begin
                 let fn = get-filler(#"calc-fn", filler);
                 (fn
                   & begin
                       let new-filler = fn(filler, mop);
                       (new-filler & add-role-filler(role, mop, new-filler));
                     end);
               end));
     end;
end method get-filler;

define method path-filler (path, mop)
  for (role(in: path)) #"always"; (mop := get-filler(role, mop)); end for
   & mop;
end method path-filler;

define method slots-abstp (mop, slots)
  abst-mopp(mop) & ~ empty?(mop-slots(mop))
   & for (slot(in: mop-slots(mop)))
       #"always";
       satisfiedp(slot-filler(slot), get-filler(slot-role(slot), slots),
                  slots);
     end for;
end method slots-abstp;

define method satisfiedp (constraint, filler, slots)
  let _that = #f;
  if (_that := empty?(constraint))
    _that;
  elseif (patternp(constraint))
    (inherit-filler(#"abst-fn", constraint))(constraint, filler, slots);
  elseif (_that := abstp(constraint, filler))
    _that;
  elseif (instance-mopp(constraint))
    empty?(filler);
  elseif (filler)
    slots-abstp(constraint, filler);
  else
    #f;
  end if;
end method satisfiedp;

define method mop-includesp (mop1, mop2)
  mop-type(mop1) == mop-type(mop2)
   & for (slot(in: mop-slots(mop2)))
       #"always";
       slot-filler(slot) == get-filler(slot-role(slot), mop1);
     end for
   & mop1;
end method mop-includesp;

define method mop-equalp (mop1, mop2)
  mop-includesp(mop2, mop1) & mop-includesp(mop1, mop2);
end method mop-equalp;

// 
// (DEFUN GET-TWIN (MOP)
//  (FOR (ABST :IN (MOP-ABSTS MOP))
//     :FIRST (FOR (SPEC :IN (MOP-SPECS ABST))
//               :WHEN (NOT (EQL SPEC MOP))
//               :FIRST (MOP-EQUALP SPEC MOP))))
//
define method get-twin (mop)
  for (abst(in: mop-absts(mop)))
    #"first";
    for (spec(in: mop-specs(abst)))
      #"when";
      ~ (spec == mop);
      #"first";
      mop-includesp(spec, mop) & (~ groupp(mop) | mop-includesp(mop, spec))
       & spec;
    end for;
  end for;
end method get-twin;

define method refine-instance (instance)
  for (abst(in: mop-absts(instance)))
    #"when";
    mops-abstp(mop-specs(abst), instance);
    #"first";
    unlink-abst(instance, abst);
    refine-instance(instance);
  end for;
end method refine-instance;

define method mops-abstp (mops, instance)
  ~ empty?(for (mop(in: mops))
             #"when";
             slots-abstp(mop, instance);
             #"save";
             link-abst(instance, mop);
           end for);
end method mops-abstp;

define method install-instance (instance)
  refine-instance(instance);
  let twin = get-twin(instance);
  if (twin)
    remove-mop(instance);
    twin;
  elseif (has-legal-absts-p(instance))
    instance;
  else
    remove-mop(instance);
    #f;
  end if;
end method install-instance;

define method has-legal-absts-p (instance)
  for (abst(in: mop-absts(instance)))
    #"when";
    ~ legal-abstp(abst, instance);
    #"do";
    unlink-abst(instance, abst);
  end for;
  mop-absts(instance);
end method has-legal-absts-p;

define method legal-abstp (abst, instance)
  mop-slots(abst)
   & for (spec(in: mop-specs(abst))) #"always"; instance-mopp(spec); end for;
end method legal-abstp;

define method install-abstraction (mop)
  let twin = get-twin(mop);
  if (twin) remove-mop(mop); twin; else reindex-siblings(mop); end if;
end method install-abstraction;

define method reindex-siblings (mop)
  for (abst(in: mop-absts(mop)))
    #"do";
    for (spec(in: mop-specs(abst)))
      #"when";
      instance-mopp(spec) & slots-abstp(mop, spec);
      #"do";
      unlink-abst(spec, abst);
      link-abst(spec, mop);
    end for;
  end for;
  mop;
end method reindex-siblings;

define method slots->mop (slots, absts, must-work)
  insist(slots->mop, ~ empty?(absts),
         for (abst(in: absts)) #"always"; mopp(abst); end for);
  empty?(slots) & empty?(tail(absts)) & head(absts)
   | begin
       let type = (slots & not(instance?(head(slots), <list>)) & head(slots));
       (type & (slots := tail(slots)));
       let mop = new-mop(#f, absts, type, slots);
       let result
           = if (instance-mopp(mop))
               install-instance(mop);
             else
               install-abstraction(mop);
             end if;
       insist(slots->mop, (result | empty?(must-work)));
       result;
     end;
end method slots->mop;

// LTD: No macros.
#"defmop";

define method forms->slots (slot-forms)
  for (slot-form(in: slot-forms))
    #"save";
    if (not(instance?(slot-form, <list>)))
      slot-form;
    else
      make-slot(slot-role(slot-form),
                begin
                  let abst = head(tail(slot-form));
                  insist(forms->slots, not(instance?(abst, <list>)));
                  abst
                   & slots->mop(forms->slots(tail(tail(slot-form))),
                                list(abst), #t);
                end);
    end if;
  end for;
end method forms->slots;

define method group-size (x)
  groupp(x) & size(mop-slots(x));
end method group-size;

define method group->list (group)
  group & insist(group->list, groupp(group))
   & for (index(in: make-m-n(1, group-size(group))))
       #"filter";
       role-filler(index, group);
     end for;
end method group->list;

define method list->group (l)
  if (empty?(l))
    #"i-m-empty-group";
  else
    slots->mop(for (x(in: l))
                 i(in: make-m-n(1, size(l)));
                 #"save";
                 make-slot(i, x);
               end for,
               #(#"m-group"), #t);
  end if;
end method list->group;

define method make-m-n (m, n)
  insist(make-m-n, instance?(m, <integer>), instance?(n, <integer>));
  if (m == n)
    list(n);
  elseif (m < n)
    pair(m, make-m-n(m + 1, n));
  else
    pair(m, make-m-n(m - 1, n));
  end if;
end method make-m-n;

define method dah (mop)
  print(tree->list(mop, specs->list, #f), *standard-output*, pretty: #t);
end method dah;

define method dph (mop)
  print(tree->list(mop, slots->forms, #f), *standard-output*, pretty: #t);
end method dph;

define method specs->list (mop, visited)
  for (spec(in: mop-specs(mop)))
    #"save";
    tree->list(spec, specs->list, visited);
  end for;
end method specs->list;

define method slots->forms (mop, visited)
  for (slot(in: mop-slots(mop)))
    #"save";
    pair(slot-role(slot), mop->form(slot-filler(slot), visited));
  end for;
end method slots->forms;

define method mop->form (mop, visited)
  tree->list(mop, slots->forms, visited);
end method mop->form;

define method tree->list (mop, fn, visited)
  if (member?(mop, visited))
    list(mop);
  else
    visited := pair(mop, visited);
    bq-cons(mop, fn(mop, visited));
  end if;
end method tree->list;

define method constraint-fn (constraint, filler, slots)
  #t;
end method constraint-fn;

define method not-constraint (constraint, filler, slots)
  insist(not-constraint, ~ empty?(filler));
  ~ satisfiedp(get-filler(#"object", constraint), filler, slots);
end method not-constraint;

define method get-sibling (pattern, mop)
  for (abst(in: mop-absts(mop)))
    #"first";
    for (spec(in: mop-specs(abst)))
      #"when";
      instance-mopp(spec) & ~ (spec == mop)
       & ~ abstp(#"m-failed-solution", spec);
      #"first";
      spec;
    end for;
  end for;
end method get-sibling;

