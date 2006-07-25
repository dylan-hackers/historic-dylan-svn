//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Below.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
// 
// 	Note		This entire file is conditional on #+dtp-trace
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Show-Below
define generic show-below (object, #key answer, #"#all-keys") ;

// ----------------------------------------------------------------------------
define method show-below (object :: <proof>, #key #all-keys)
  for (child in objects-below(object))
    output(child,
           parent-id: if (display-query-p(object)) dot-id-of(object); end if);
  end for;
end method show-below;

// ----------------------------------------------------------------------------
define method show-below (object :: <answer>, #key #all-keys)
  output(c-just-conjunction(answer-justification(object)),
         parent-id: if (display-query-p(answer-proof(object)))
                      dot-id-of(object);
                    end if,
         answer: c-just-answer(answer-justification(object)));
end method show-below;

// ----------------------------------------------------------------------------
define method show-below (object :: <dtp-subgoal>, #key answer = #f,
                          parent-id = #f, my-id = #f, record = #f,
                          previous-sg-hidden = #f, #"#all-keys")
  if (~ *graphic-display*) inc!(*depth*); end if;
  if (~ my-id) my-id := dot-id-of(object); end if;
  if (answer)
    if (instance?(answer, <answer>))
      let just = answer-justification(answer);
      let hidden = #f;
      hidden := hidden-subgoal(parent-id) & ~ previous-sg-hidden;
      output(just, link-label: #f, record: if (hidden) record; end if,
             previous-sg-hidden: hidden,
             parent-id: if (hidden) parent-id; else my-id; end if);
    end if;
  else
    if (~ cl-find(object, *explored*))
      push!(object, *explored*);
      for (child in objects-below(object))
        let label = #f;
        if (instance?(child, <dtp-conjunction>)
             | instance?(child, <r-justification>))
          label := binding-label-between(object, child);
        end if;
        output(child, parent-id: my-id, link-label: label);
      end for;
    end if;
  end if;
  if (~ *graphic-display*) dec!(*depth*); end if;
end method show-below;

// ----------------------------------------------------------------------------
define method show-below (object :: <dtp-conjunction>, #key answer = #f,
                          parent-id = #f, my-id = #f, record = #f,
                          link-label = #f, previous-sg-hidden = #f,
                          #"#all-keys")
  if (~ hidden-conjunction(object))
    if (my-id)
      parent-id := my-id;
    elseif (instance?(object, <dtp-forked-conjunction>)
             & ~ *display-blocked-separately*)
      parent-id := dot-id-of(object.parent-conjunction);
    else
      parent-id := dot-id-of(object);
    end if;
    link-label := #f;
  end if;
  if (answer)
    if (instance?(answer, <answer>))
      let sg-justs
          = remove(answer-justification(answer),
                   complement(s-justification-p));
      for (just in sg-justs,
           o-record = if (hidden-conjunction(object))
                        record;
                      else
                        s-just-conjunct-number(just);
                      end if then if (hidden-conjunction(object))
                                    record;
                                  else
                                    s-just-conjunct-number(just);
                                  end if,
           o-hidden = if (hidden-conjunction(object))
                        previous-sg-hidden;
                      else
                        #f;
                      end if then if (hidden-conjunction(object))
                                    previous-sg-hidden;
                                  else
                                    #f;
                                  end if)
        output(s-just-subgoal(just), parent-id: parent-id, record: o-record,
               previous-sg-hidden: o-hidden, link-label: link-label,
               answer: s-just-answer(just));
      end for;
    end if;
  else
    for (child in objects-below(object))
      output(child, parent-id: parent-id, link-label: link-label);
    end for;
  end if;
end method show-below;

// ----------------------------------------------------------------------------
define method show-below (object :: <dtp-conjunct>, #key parent-id = #f,
                          link-label = #f, #"#all-keys")
  if (instance?(dot-id-to-object(parent-id), <dtp-conjunction>))
    let parent-c = object.parent-conjunction;
    let num = find-key(parent-c.list, curry(\==, object));
    for (child in objects-below(object))
      output(child, parent-id: parent-id, record: num,
             link-label: link-label);
    end for;
  else
    for (child in objects-below(object))
      output(child, parent-id: parent-id, link-label: link-label);
    end for;
  end if;
end method show-below;

// ----------------------------------------------------------------------------
define method show-below (object :: <c-justification>, #key parent-id = #f,
                          record = #f, link-label = #f,
                          previous-sg-hidden = #f, #"#all-keys")
  output(c-just-conjunction(object), parent-id: parent-id, record: record,
         link-label: link-label, answer: c-just-answer(object),
         previous-sg-hidden: previous-sg-hidden);
end method show-below;

// ----------------------------------------------------------------------------
define method show-below (object :: <sg-cache-justification>, #key #all-keys)
  output(sg-cache-just-subgoal(object));
end method show-below;

// ----------------------------------------------------------------------------
define method show-below (object :: <justification>, #key answer = #f,
                          #"#all-keys")
  // Nothing is below remaining justifications
  #f;
end method show-below;

// ----------------------------------------------------------------------------
// 
// 	Objects-Below
define generic objects-below (object) ;

// ----------------------------------------------------------------------------
define method objects-below (object :: <proof>)
  hide-single-conjunctions(sort!(remove-uninitialized(concatenate(proof-used-conjunctions(object),
                                                                  proof-blocked-conjunctions(object),
                                                                  proof-query-conjunctions(object))),
                                 test: method (x, y)
                                       (method (c) c.order; end method)(x)
                                        < (method (c) c.order; end method)(y);
                                       end method));
end method objects-below;

// ----------------------------------------------------------------------------
define method objects-below (object :: <dtp-subgoal>)
  let objs = #f;
  objs
   := concatenate(object.used-inferences, object.blocked-conjunctions,
                  if (instance?(object.inferences, <list>))
                    //  Unless :UNINITIALIZED
                    choose(complement(virgin-p), object.inferences);
                  end if);
  objs
   := sort!(objs,
            test: method (x, y)
                    (method (c) c.order; end method)(x)
                     < (method (c) c.order; end method)(y);
                  end method);
  if (objs)
    begin
      let reductions = object.answers;
      reductions := remove(reductions, complement(answer-context));
      reductions := map(answer-justification, reductions);
      reductions
       := remove(reductions,
                 complement(method (just)
                              r-just-leaf-subgoal(just) == object;
                            end method));
      concatenate(reductions, objs);
    end;
    //  Success cache
    elseif (objs := map(answer-justification, object.answers))
    objs;
    //  Failure cache
    elseif (object.failure-explanation)
    list(object.failure-explanation);
  end if;
end method objects-below;

// ----------------------------------------------------------------------------
define method objects-below (object :: <dtp-conjunction>)
  let conjuncts = #f;
  conjuncts := object.list;
  if (instance?(object, <dtp-forked-conjunction>))
    conjuncts := nth-tail(conjuncts, object.top-conjunct);
  end if;
  conjuncts;
end method objects-below;

// ----------------------------------------------------------------------------
define method objects-below (object :: <dtp-conjunct>)
  // If forked conjunction, then new subgoals below, else all subgoals below
  let parent-c = object.parent-conjunction;
  let subgoals = #f;
  subgoals := subgoals-below-conjunct(object);
  if (~ *display-blocked-separately*
       & instance?(parent-c, <dtp-forked-conjunction>))
    let un-f-c = parent-c.parent-conjunction;
    let num = find-key(parent-c.list, curry(\==, object));
    let un-f-conj = #f;
    let un-f-subgoals = #f;
    un-f-conj := un-f-c.list[num];
    un-f-subgoals := subgoals-below-conjunct(un-f-conj);
    subgoals := set-difference(subgoals, un-f-subgoals);
  end if;
  subgoals;
end method objects-below;

define method subgoals-below-conjunct (conjunct)
  cl-remove-duplicates(remove-uninitialized(concatenate(conjunct
                                                        .used-subgoals,
                                                        list(conjunct
                                                             .subgoal))));
end method subgoals-below-conjunct;

// ----------------------------------------------------------------------------
define method hide-single-conjunctions (conjunctions)
  reduce1(concatenate,
          map(method (c)
                if (size(c.list) == 1)
                  objects-below(first(c.list));
                else
                  list(c);
                end if;
              end method,
              conjunctions));
end method hide-single-conjunctions;

// ----------------------------------------------------------------------------
define method remove-uninitialized (l)
  remove(l, complement(method (x) instance?(x, <dtp-object>); end method));
end method remove-uninitialized;

// ----------------------------------------------------------------------------
"eof";

