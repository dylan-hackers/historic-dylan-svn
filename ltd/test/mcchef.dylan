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
define method chef (slots)
  let instance = slots->mop(slots, #(#"m-recipe"), #t);
  get-filler(#"steps", instance) & instance;
end method chef;

define method mop-subst (new, old, mop)
  if (mop == old)
    new;
  elseif (empty?(mop-slots(mop)))
    mop;
  else
    begin
      let slots = mop-slots(mop);
      let new-slots = slots-subst(new, old, slots);
      if (new-slots = slots)
        mop;
      elseif (groupp(mop))
        list->group(for (slot(in: new-slots))
                      #"save";
                      slot-filler(slot);
                    end for);
      else
        raise-abst(forms->slots(new-slots), mop);
      end if;
    end;
  end if;
end method mop-subst;

define method slots-subst (new, old, slots)
  for (slot(in: slots))
    #"filter";
    begin
      let filler = mop-subst(new, old, slot-filler(slot));
      filler & make-slot(slot-role(slot), filler);
    end;
  end for;
end method slots-subst;

define method raise-abst (slots, mop)
  let abst
      = for (abst(in: mop-all-absts(mop)))
          #"when";
          slots-abstp(abst, slots);
          #"first";
          abst;
        end for;
  abst & slots->mop(slots, list(abst), #f);
end method raise-abst;

define method get-precons (ingred)
  format-out("\n----------------");
  format-out("\nGetting preconditions for %=", ingred);
  ingred & slots->mop(list(list(#"ingred", ingred)), #(#"m-precons"), #f);
end method get-precons;

define method make-mop (pattern, mop)
  let slots
      = for (slot(in: mop-slots(pattern)))
          #"when";
          ~ (slot-role(slot) == #"mop");
          #"save";
          slot;
        end for;
  slots->mop(replace-slots(slots, mop), list(get-filler(#"mop", pattern)),
             #t);
end method make-mop;

define method replace-slots (slots, mop)
  for (slot(in: slots))
    #"save";
    list(slot-role(slot),
         begin
           let filler = slot-filler(slot);
           if (abstp(#"m-role", filler))
             role-filler(filler, mop);
           elseif (abstp(#"m-path", filler))
             path-filler(group->list(filler), mop);
           else
             filler;
           end if;
         end);
  end for;
end method replace-slots;

define method compare-constraint (constraint, filler, slots)
  (get-filler(#"compare-fn", constraint))(indirect-filler(#"role",
                                                          constraint,
                                                          filler),
                                          indirect-filler(#"to",
                                                          constraint,
                                                          slots));
end method compare-constraint;

define method indirect-filler (role, mop, slots)
  let path-mop = get-filler(role, mop);
  path-filler(if (empty?(path-mop))
                #f;
              elseif (abstp(#"m-path", path-mop))
                group->list(path-mop);
              else
                list(path-mop);
              end if,
              slots);
end method indirect-filler;

define method not-eql (x, y) x & y & ~ (x == y); end method not-eql;

define method group-member (mop, group)
  groupp(group)
   & for (slot(in: mop-slots(group)))
       #"first";
       slot-filler(slot) == mop;
     end for;
end method group-member;

define method group-splice (new, old, group)
  list->group(for (mop(in: group->list(group)))
                #"splice";
                if (mop == old) new; else list(mop); end if;
              end for);
end method group-splice;

define method group-insert (mop, group)
  if (empty?(mop))
    group;
  elseif (group-member(mop, group))
    group;
  else
    list->group(concatenate(group->list(group), list(mop)));
  end if;
end method group-insert;

define method add-precons (precon-mop, steps-slots)
  if (empty?(precon-mop))
    steps-slots;
  else
    format-out("\nAdding preconditions to recipe");
    for (slot(in: steps-slots))
      #"save";
      merge-step(precon-mop, slot);
    end for;
  end if;
end method add-precons;

define method merge-step (precon-mop, slot)
  let role = slot-role(slot);
  let old-filler = slot-filler(slot);
  let new-filler = group-insert(get-filler(role, precon-mop), old-filler);
  if (new-filler == old-filler)
    slot;
  else
    make-slot(role, new-filler);
  end if;
end method merge-step;

define method adapt-steps (pattern, mop)
  let recipe = get-filler(#"old", mop);
  format-out("\n----------------");
  format-out("\nAdapting the steps in %=", recipe);
  let steps = mop-slots(get-filler(#"steps", recipe));
  for (ingred-role(in: #(#"meat", #"vege", #"vege2")))
    #"always";
    (steps
      := subst-ingred(get-filler(ingred-role, mop),
                      get-filler(ingred-role, recipe), steps));
  end for
   & slots->mop(steps, #(#"m-recipe-steps"), #f);
end method adapt-steps;

define method subst-ingred (new-ingred, old-ingred, steps)
  if (empty?(new-ingred) | new-ingred == old-ingred)
    steps;
  else
    add-precons(get-precons(new-ingred),
                if (empty?(old-ingred))
                  steps;
                else
                  format-out("\nSubstituting %= for %=", new-ingred,
                             old-ingred);
                  slots-subst(new-ingred, old-ingred, steps);
                end if);
  end if;
end method subst-ingred;

define method chef-repair (slots)
  link-abst(role-filler(#"solution", slots), #"m-failed-solution");
  let instance = slots->mop(slots, #(#"m-repair"), #t);
  get-filler(#"repaired-solution", instance) & instance;
end method chef-repair;

define method isa-constraint (constraint, filler, slots)
  abstp(role-filler(role-filler(#"role", constraint), slots), filler);
end method isa-constraint;

define method reset-role-filler (role, mop, new-value)
  let old-value = role-filler(role, mop);
  insist(reset-role-filler, ~ empty?(old-value));
  if (old-value == new-value)
    mop;
  else
    raise-abst(for (slot(in: mop-slots(mop)))
                 #"save";
                 if (slot-role(slot) == role)
                   make-slot(role, new-value);
                 else
                   slot;
                 end if;
               end for,
               mop);
  end if;
end method reset-role-filler;

define method split-step (pattern, mop)
  let step = path-filler(#(#"explanation", #"cause"), mop);
  let object = path-filler(#(#"explanation", #"failure", #"object"), mop);
  let step-object = role-filler(#"object", step);
  list->group(list(reset-role-filler(#"object", step, object),
                   reset-role-filler(#"object", step,
                                     group-splice(#f, object, step-object))));
end method split-step;

define method apply-repair (pattern, mop)
  let repair = get-filler(#"repair", mop);
  let cause = path-filler(#(#"explanation", #"cause"), mop);
  let recipe = role-filler(#"solution", mop);
  let steps = role-filler(#"steps", recipe);
  reset-role-filler(#"steps", recipe,
                    for (slot(in: mop-slots(steps)))
                      #"when";
                      group-member(cause, slot-filler(slot));
                      #"first";
                      reset-role-filler(slot-role(slot),
                                        steps,
                                        group-splice(group->list(repair),
                                                     cause,
                                                     slot-filler(slot)));
                    end for);
end method apply-repair;

define method generalize-repair (repair, input-roles)
  let solution = get-filler(#"repaired-solution", repair);
  let slots
      = generalize-slots(solution, input-roles,
                         path-filler(#(#"explanation", #"mapping"), repair));
  let absts = mop-absts(solution);
  for (slot(in: slots))
    #"do";
    slots->mop(forms->slots(list(list(slot-role(slot), #"m-not",
                                      list(#"object", slot-filler(slot))))),
               absts, #t);
  end for;
  slots->mop(slots, absts, #t);
end method generalize-repair;

define method generalize-slots (mop, roles, maps)
  for (role(in: roles))
    #"filter";
    generalize-slot(role, role-filler(role, mop), maps);
  end for;
end method generalize-slots;

define method generalize-slot (role, mop, maps)
  mop
   & begin
       let abst = generalize-mop(mop, maps);
       (abst & make-slot(role, abst));
     end;
end method generalize-slot;

define method generalize-mop (mop, maps)
  for (slot(in: mop-slots(maps)))
    #"when";
    role-filler(#"spec", slot-filler(slot)) == mop;
    #"first";
    role-filler(#"abst", slot-filler(slot));
  end for;
end method generalize-mop;

define method chef-explain (mop)
  slots->mop(list(#"instance", list(#"failure", mop),
                  list(#"cause", *bad-step*), #(#"rule", #"m-rule"),
                  list(#"mapping",
                       slots->mop(forms->slots(#(#(1,
                                                   #"m-map",
                                                   #"instance",
                                                   #(#"abst", #"m-meat"),
                                                   #(#"spec", #"i-m-beef")),
                                                 #(2,
                                                   #"m-map",
                                                   #"instance",
                                                   #(#"abst",
                                                     #"m-crisp-vegetable"),
                                                   #(#"spec",
                                                     #"i-m-broccoli")))),
                                  #(#"m-map-group"), #t))),
             #(#"m-explanation"), #t);
end method chef-explain;

define method run-chef (slots)
  let recipe = chef(slots);
  recipe
   & begin
       format-out("\n----------------");
       format-out("\nThe steps in %= are:", recipe);
       dph(role-filler(#"steps", recipe));
       recipe;
     end;
end method run-chef;

define method chef1 ()
  *bad-recipe*
   := run-chef(#(#(#"meat", #"i-m-beef"), #(#"vege", #"i-m-broccoli"),
                 #(#"taste", #"i-m-hot"), #(#"style", #"i-m-stir-fry")));
  *bad-step* := path-filler(#(#"steps", #"stir-fry-steps", 1), *bad-recipe*);
  *bad-recipe*;
end method chef1;

define method chef2 ()
  *bad-recipe-explanation*
   := chef-explain(slots->mop(#(#(#"state", #"i-m-soggy"),
                                #(#"object", #"i-m-broccoli")),
                              #(#"m-failure"), #t));
end method chef2;

define method chef3 ()
  *recipe-repair*
   := chef-repair(list(list(#"solution", *bad-recipe*),
                       list(#"explanation", *bad-recipe-explanation*)));
  *good-recipe* := role-filler(#"repaired-solution", *recipe-repair*);
end method chef3;

define method chef4 ()
  generalize-repair(*recipe-repair*,
                    #(#"meat", #"vege", #"vege2", #"taste", #"style"));
end method chef4;

define method chef5 ()
  run-chef(#(#(#"meat", #"i-m-chicken"), #(#"vege", #"i-m-snow-peas"),
             #(#"style", #"i-m-stir-fry")));
end method chef5;

define method chef6 ()
  run-chef(#(#(#"meat", #"i-m-beef"), #(#"vege", #"i-m-green-peppers"),
             #(#"style", #"i-m-stir-fry")));
end method chef6;

define method chef-demo ()
  format-out("\n----------------");
  format-out("\nBeef and broccoli recipe");
  insist(chef-demo, chef1());
  format-out("\n----------------");
  format-out("\nKludging soggy broccoli failure");
  link-abst(*bad-recipe*, #"m-failed-solution");
  format-out("\nCause is %=", *bad-step*);
  format-out("\nKludging explanation");
  insist(chef-demo, chef2());
  format-out("\nExplanation is %=", *bad-recipe-explanation*);
  format-out("\n----------------");
  format-out("\nTrying to repair %=", *bad-recipe*);
  insist(chef-demo, chef3());
  format-out("\nRepaired beef and broccoli recipe is %=", *good-recipe*);
  format-out("\n----------------");
  format-out("\nThe repaired steps are:");
  dph(role-filler(#"steps", *good-recipe*));
  format-out("\n----------------");
  format-out("\nGeneralizing repair");
  insist(chef-demo, chef4());
  format-out("\nNew hierarchies:");
  dah(#"m-recipe");
  dah(#"m-failed-solution");
  format-out("\n----------------");
  format-out("\nChicken and snow-peas recipe");
  insist(chef-demo, chef5());
  format-out("\n----------------");
  format-out("\nBeef and green peppers recipe");
  insist(chef-demo, chef6());
end method chef-demo;

