// *****************************************************************
//   MICRO-TALESPIN: A STORY GENERATOR
// 
//   A reconstruction, in Common Lisp, of James Meehan's program in
//   _Inside_Computer_Understanding:_Five_Programs_Plus_Miniatures_
//   Roger Schank and Christopher Riesbeck (eds.)
// 
//   Warren Sack                 
//   MIT Media Lab
//   20 Ames Street, E15-486
//   Cambridge MA 02139
//   wsack@media.mit.edu
// 
//   October 1992
// 
//   I translated Micro-Talespin into Common Lisp as a
//   "literature review exercise":  I wanted to see and play
//   with storyteller systems that had been written in the past.
//   I am currently working on creating storyteller systems which
//   produce not only text (as Micro-Talespin does) but also
//   audio and video.  If you are working on a similar project
//   I'd love to hear from you.  I can be reached at the
//   above address.
// 
// *****************************************************************
//   Standard definition of put.
// LTD: No macros.
#"put";

//   Definitions necessary for pattern variables.
define class <pcvar> (<object>)
  slot pcvar-id, init-keyword: #"pcvar-id";
end class <pcvar>;

define method print-pcvar (var, stream, depth)
  format(stream, "?%=", var.pcvar-id);
end method print-pcvar;

// LTD: Function SET-MACRO-CHARACTER not yet implemented.
set-macro-character('?',
                    method (stream, char)
                      make-pcvar(id: // LTD: Function READ not yet implemented.
                                     read(stream, #t, #f, #t));
                    end method,
                    #t);

//   Definition of Globals
define variable *personae* = #f;

define variable *goals* = #f;

define variable *all-locations* = #f;

define variable *all-objects* = #f;

//   This is the initial data base.  It can be extended before 
//   running a story.
define variable *init-facts* = #f;

define method init-facts ()
  *init-facts*
   := #(#(#"world", #(#"loc", #(#"actor", #"joe"), #(#"val", #"cave"))),
        #(#"joe", #(#"loc", #(#"actor", #"joe"), #(#"val", #"cave"))),
        #(#"world",
          #(#"loc", #(#"actor", #"irving"), #(#"val", #"oak-tree"))),
        #(#"irving",
          #(#"loc", #(#"actor", #"irving"), #(#"val", #"oak-tree"))),
        #(#"joe", #(#"loc", #(#"actor", #"irving"), #(#"val", #"oak-tree"))),
        #(#"world", #(#"loc", #(#"actor", #"water"), #(#"val", #"river"))),
        #(#"joe", #(#"loc", #(#"actor", #"water"), #(#"val", #"river"))),
        #(#"world", #(#"loc", #(#"actor", #"honey"), #(#"val", #"elm-tree"))),
        #(#"irving",
          #(#"loc", #(#"actor", #"honey"), #(#"val", #"elm-tree"))),
        #(#"world", #(#"loc", #(#"actor", #"worm"), #(#"val", #"ground"))),
        #(#"joe", #(#"loc", #(#"actor", #"worm"), #(#"val", #"ground"))),
        #(#"irving", #(#"loc", #(#"actor", #"joe"), #(#"val", #"cave"))),
        #(#"world", #(#"loc", #(#"actor", #"fish"), #(#"val", #"river"))),
        #(#"irving", #(#"loc", #(#"actor", #"fish"), #(#"val", #"river"))));
end method init-facts;

//   init-world sets up a bunch of facts such as Joe is a bear, birds
//   eat worms, and so on.  The variable *init-facts* contains location
//   and relationship facts, along with which character knows them.
define method init-world ()
  put(#"joe", #"is-a", #"bear");
  put(#"joe", #"home", #"cave");
  put(#"irving", #"is-a", #"bird");
  put(#"irving", #"home", #"tree");
  put(#"bear", #"food", #(#"honey", #"berries", #"fish"));
  put(#"bird", #"food", #(#"worm"));
  *personae* := #(#"joe", #"irving");
  *goals* := #(#"hungry", #"thirsty");
  *all-locations*
   := #(#"cave", #"oak-tree", #"elm-tree", #"ground", #"river");
  *all-objects*
   := concatenate(*all-locations*,
                  #(#"honey", #"berries", #"fish", #"worm", #"water"));
  let list92543 = pair(#"world", *personae*);
  begin
    do(method (persona)
         put(persona, #"facts", #f);
         put(persona, #"goals", #f);
         put(persona, #"demons", #f);
       end method,
       list92543);
    list92543;
  end;
  begin
    do(method (fact) now-knows(head(fact), second(fact), #t); end method,
       *init-facts*);
    *init-facts*;
  end;
end method init-world;

//   The success of asking something depends upon whether the other person
//   is honest and likes you.
define method ask-plan (actor, agent, action)
  list(#"and",
       list(#"not",
            apply(list, #"relate", list(#"quote", actor),
                  list(#"quote", agent), list(#"quote", actor),
                  #(#(#"quote", #"deceive")))),
       apply(list, #"relate", list(#"quote", actor), list(#"quote", actor),
             list(#"quote", agent), #(#(#"quote", #"like"))),
       list(#"tell", list(#"quote", actor), list(#"quote", agent),
            list(#"question", list(#"quote", action))));
end method ask-plan;

//   The success of bargaining with someone by giving them food depends
//   on whether the other person is honest, you don't already have the
//   goal of getting the food you're going to bargain with, and you can
//   get the food to the other person.
define method bargain-plan (actor, agent, action)
  let atrans-food = atrans(actor, #"food", agent, actor);
  list(#"and",
       list(#"not",
            apply(list, #"relate", list(#"quote", actor),
                  list(#"quote", agent), list(#"quote", actor),
                  #(#(#"quote", #"deceive")))),
       list(#"not",
            list(#"knows", list(#"quote", actor),
                 apply(list, #"has", list(#"quote", agent),
                       #(#(#"quote", #"food"))))),
       list(#"not",
            list(#"has-goal-of", list(#"quote", actor),
                 apply(list, #"has", list(#"quote", actor),
                       #(#(#"quote", #"food"))))),
       list(#"doit",
            list(#"mbuild", list(#"quote", actor),
                 list(#"cause", list(#"quote", atrans-food),
                      list(#"maybe", list(#"quote", action))))),
       list(#"tell", list(#"quote", actor), list(#"quote", agent),
            list(#"question",
                 list(#"cause", list(#"quote", atrans-food),
                      list(#"future", list(#"quote", action))))),
       apply(list, #"dcont", list(#"quote", actor), #(#(#"quote", #"food"))),
       list(#"dprox", list(#"quote", actor), list(#"quote", actor),
            list(#"quote", agent)),
       list(#"doit", list(#"quote", atrans-food)),
       list(#"is-true", list(#"quote", action)));
end method bargain-plan;

//   The success of threatening depends upon whether you dominate
//   the other person.
define method threat-plan (actor, agent, action)
  list(#"and",
       list(#"not",
            apply(list, #"relate", list(#"quote", actor),
                  list(#"quote", agent), list(#"quote", actor),
                  #(#(#"quote", #"dominate")))),
       list(#"tell", list(#"quote", actor), list(#"quote", agent),
            list(#"cause", list(#"negate", list(#"quote", action)),
                 list(#"future",
                      list(#"propel", list(#"quote", actor),
                           #(#"quote", #"hand"), list(#"quote", agent))))),
       list(#"or", list(#"is-true", list(#"quote", action)),
            list(#"and",
                 list(#"doit",
                      list(#"propel", list(#"quote", actor),
                           #(#"quote", #"hand"), list(#"quote", agent))),
                 list(#"is-true", list(#"quote", action)))));
end method threat-plan;

//   Set the storytelling in the past tense.
define variable *default-tense* = #"past";

//   micro-talespin-demo variables for sample stories
//   No plot: joe gets a drink of water.
define variable *story1* = #(#"joe", #"thirsty");

//   irving kills joe.
define variable *story2* =
  #(#"irving", #"thirsty",
    #(#"irving",
      #(#"like", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"neg")))),
    #(#"irving",
      #(#"dominate", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"neg")))),
    #(#"irving",
      #(#"deceive", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"pos")))),
    #(#"irving",
      #(#"like", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"neg")))),
    #(#"joe",
      #(#"deceive", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"neg")))));

//   joe is frustrated at every turn.
define variable *story3* =
  #(#"joe", #"hungry",
    #(#"joe",
      #(#"like", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"neg")))),
    #(#"joe",
      #(#"dominate", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"pos")))));

//   joe and irving strike a deal.
define variable *story4* =
  #(#"joe", #"hungry",
    #(#"world", #(#"hungry", #(#"actor", #"irving"), #(#"mode", #(#"pos")))),
    #(#"joe",
      #(#"like", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"pos")))),
    #(#"joe",
      #(#"deceive", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"neg")))),
    #(#"joe",
      #(#"like", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"pos")))),
    #(#"irving",
      #(#"like", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"pos")))),
    #(#"irving",
      #(#"dominate", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"neg")))),
    #(#"irving",
      #(#"deceive", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"neg")))));

//   joe tricks irving
define variable *story5* =
  #(#"irving", #"thirsty",
    #(#"irving",
      #(#"like", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"pos")))),
    #(#"irving",
      #(#"like", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"pos")))),
    #(#"irving",
      #(#"deceive", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"neg")))),
    #(#"irving",
      #(#"dominate", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"pos")))),
    #(#"world", #(#"hungry", #(#"actor", #"joe"), #(#"mode", #(#"pos")))),
    #(#"joe",
      #(#"like", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"neg")))),
    #(#"joe",
      #(#"deceive", #(#"actor", #"joe"), #(#"to", #"irving"),
        #(#"mode", #(#"pos")))));

//   This is an interactive version of *story4* and/or *story5*
define variable *story6* =
  #(#"joe", #"hungry",
    #(#"joe",
      #(#"like", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"pos")))),
    #(#"joe",
      #(#"dominate", #(#"actor", #"irving"), #(#"to", #"joe"),
        #(#"mode", #(#"neg")))));

//   Declare globals used in forward-chaining through goals and plans.
define variable *actions* = #f;

define variable *plans* = #f;

define variable *conseqs* = #f;

define method micro-talespin ()
  init-facts();
  let main-character = pick-one(#"character", *personae*);
  let problem = pick-one(#"problem", *goals*);
  format-out("\nOnce upon a time ...");
  init-world();
  format-out("\nOne day,");
  assert-fact(mloc(#"world", state(main-character, problem, #"pos")));
  format-out("\nThe end.");
end method micro-talespin;

//   micro-talespin-demo lets you predefine more facts for a story.
//   story should be a list of the form (character problem fact fact ...)
//   where
//      character is either joe or irving,
//      problem is either hunger or thirst,
//      facts have the for (character 'CD-form).  The character field
//            says who knows this fact.
define method micro-talespin-demo (story)
  init-facts();
  *init-facts* := concatenate(*init-facts*, tail(tail(story)));
  let main-character = head(story);
  let problem = second(story);
  format-out("\nOnce upon a time ...");
  init-world();
  format-out("\nOne day, ");
  assert-fact(mloc(#"world", state(main-character, problem, #"pos")));
  format-out("\nThe end.");
end method micro-talespin-demo;

//   pick-one is used to get the character and problem from the terminal.
define method pick-one (name, l)
  format-out("\nChoose a %= from this list:\n%=\n> ", name, l);
  let a
      = // LTD: Function READ not yet implemented.
        read();
  if (member?(a, l)) a; else pick-one(name, l); end if;
end method pick-one;

//   goal evaluator: executes each plan until one works and the goal
//   can be removed, or until none do and the character fails to get the
//   goal.  If the goal is already true (and the actor knows that), then
//   return success immediately.  If the actor already has the goal,
//   then he's in a loop and has failed.  Otherwise, set up the goal and go.
define method goal-eval (actor, goal, plans)
  if (knows(actor, goal))
    #t;
  elseif (has-goal-of(actor, goal))
    #f;
  else
    gets-new-goal-of(actor, goal);
    if (run-plans(plans))
      forgets-goal-of(actor, goal);
      #t;
    else
      now-knows(actor, negate(future(goal)), #t);
      #f;
    end if;
  end if;
end method goal-eval;

define method run-plans (plans)
  let plan = head(plans);
  if (plan)
    if (// LTD: Function EVAL not yet implemented.
        eval(plan))
      #t;
    else
      run-plans(tail(plans));
    end if;
  end if;
end method run-plans;

//   gen-plans replicates the same plan with different objects
//   e.g., trying to get any one of the several foods with the
//   same bargaining plan.
define method gen-plans (var, possibilities, plan-form)
  map(method (possibility)
        replace-in-tree(possibility, var, plan-form);
      end method,
      possibilities);
end method gen-plans;

//   Two S-goals -- thirst and hunger:
//   To satisfy thirst, go to some water and drink it.
define method sthirst (actor)
  goal-eval(actor, state(actor, #"thirsty", #"neg"),
            list(sthirst-plan(actor)));
end method sthirst;

define method sthirst-plan (actor)
  list(#"and",
       apply(list, #"dprox", list(#"quote", actor), list(#"quote", actor),
             #(#(#"quote", #"water"))),
       list(#"doit",
            apply(list, #"ingest", list(#"quote", actor),
                  #(#(#"quote", #"water")))));
end method sthirst-plan;

//   To satisfy hunger, get some food and eat it.
define method shunger (actor)
  goal-eval(actor, state(actor, #"hungry", #"neg"),
            gen-plans(#"food", get-isa(#"food", actor), shunger-plan(actor)));
end method shunger;

define method shunger-plan (actor)
  list(#"and",
       apply(list, #"dcont", list(#"quote", actor), #(#(#"quote", #"food"))),
       list(#"doit",
            apply(list, #"ingest", list(#"quote", actor),
                  #(#(#"quote", #"food")))));
end method shunger-plan;

//   Three D-goals -- dcont, dknow, dprox:
//   To get an object: if you know someone has it, persuade them to
//   give it to you; otherwise try to find out where the object is,
//   go there and take it.
define method dcont (actor, object)
  let owner = knows-owner(actor, object);
  goal-eval(actor, has(actor, object),
            if (owner)
              list(dcont-plan1(actor, object, owner));
            else
              list(dcont-plan2(actor, object));
            end if);
end method dcont;

define method dcont-plan1 (actor, object, owner)
  list(#"persuade", list(#"quote", actor), list(#"quote", owner),
       list(#"atrans", list(#"quote", owner), list(#"quote", object),
            list(#"quote", actor), list(#"quote", owner)));
end method dcont-plan1;

define method dcont-plan2 (actor, object)
  list(#"and",
       list(#"dknow", list(#"quote", actor),
            list(#"where-is", list(#"quote", object))),
       list(#"dprox", list(#"quote", actor), list(#"quote", actor),
            list(#"quote", object)),
       list(#"doit",
            apply(list, #"atrans", list(#"quote", actor),
                  list(#"quote", object), list(#"quote", actor), #(#()))));
end method dcont-plan2;

//   To find out something: find a friend to tell you
define method dknow (actor, info)
  goal-eval(actor, mloc(actor, info),
            gen-plans(#"agent", remove(*personae*, actor),
                      dknow-plan(actor, info)));
end method dknow;

define method dknow-plan (actor, info)
  list(#"and",
       apply(list, #"knows-loc", list(#"quote", actor),
             #(#(#"quote", #"agent"))),
       list(#"or",
            list(#"is-friend-of", #(#"quote", #"agent"),
                 list(#"quote", actor)),
            list(#"not",
                 apply(list, #"relate", list(#"quote", actor),
                       #(#"quote", #"agent"), list(#"quote", actor),
                       #(#(#"quote", #"dominate"))))),
       list(#"persuade", list(#"quote", actor), #(#"quote", #"agent"),
            apply(list, #"mtrans", #(#"quote", #"agent"),
                  list(#"quote", info), list(#"quote", actor),
                  #(#(#"quote", #"agent")))));
end method dknow-plan;

//   To move an object (including yourself) to where some other
//   person or object is: get the first object (if not yourself), then
//   find out where the second object is and go there with the first
//   object.  If this doesn't work, try persuading the object to go
//   there itself.
define method dprox (actor, object, new-object)
  goal-eval(actor, is-at(object, new-object),
            list(dprox-plan1(actor, object, new-object),
                 dprox-plan2(actor, object, new-object)));
end method dprox;

define method dprox-plan1 (actor, object, new-object)
  list(#"and",
       list(#"or",
            list(#"equal", list(#"quote", actor), list(#"quote", object)),
            list(#"dprox", list(#"quote", actor), list(#"quote", actor),
                 list(#"quote", object))),
       list(#"dknow", list(#"quote", actor),
            list(#"where-is", list(#"quote", new-object))),
       list(#"or",
            list(#"equal", list(#"quote", actor), list(#"quote", object)),
            list(#"doit",
                 list(#"grasp", list(#"quote", actor),
                      list(#"quote", object)))),
       list(#"or",
            list(#"is-prox", list(#"quote", actor),
                 list(#"loc-name-of", list(#"quote", new-object))),
            list(#"doit",
                 list(#"ptrans", list(#"quote", actor),
                      list(#"quote", object),
                      list(#"knows-loc", list(#"quote", actor),
                           list(#"quote", new-object)),
                      list(#"knows-loc", list(#"quote", actor),
                           list(#"quote", actor))))),
       list(#"or",
            list(#"equal", list(#"quote", actor), list(#"quote", object)),
            list(#"doit",
                 list(#"un-grasp", list(#"quote", actor),
                      list(#"quote", object)))));
end method dprox-plan1;

define method dprox-plan2 (actor, object, new-object)
  list(#"and",
       list(#"not",
            list(#"equal", list(#"quote", actor), list(#"quote", object))),
       apply(list, #"member", list(#"quote", object), #(#"*personae*")),
       apply(list, #"peruade", list(#"quote", actor), list(#"quote", object),
             list(#"ptrans", list(#"quote", object), list(#"quote", object),
                  list(#"quote", new-object),
                  list(#"loc-name-of", list(#"quote", object))),
             #(#"goal")));
end method dprox-plan2;

//   Subgoals and plans -- persuade, ask, bargain, threaten, and tell:
//   You can persuade someone to do something by either asking them,
//   giving them food or threatening them.
define method persuade (actor, agent, action)
  goal-eval(actor, action,
            concatenate(gen-plans(#"food", get-isa(#"food", agent),
                                  bargain-plan(actor, agent, action)),
                        list(ask-plan(actor, agent, action)),
                        list(threat-plan(actor, agent, action))));
end method persuade;

//   To tell someone something, go there and say it.
define method tell (actor, agent, info)
  goal-eval(actor, mloc(agent, info), list(tell-plan(actor, agent, info)));
end method tell;

define method tell-plan (actor, agent, info)
  list(#"and",
       list(#"dprox", list(#"quote", actor), list(#"quote", actor),
            list(#"quote", agent)),
       list(#"doit",
            list(#"mtrans", list(#"quote", actor), list(#"quote", info),
                 list(#"quote", agent), list(#"quote", actor))));
end method tell-plan;

//   The simulator
//   doit adds a CD and its consequences to the data base, by calling
//   assert-fact.  mtranses with '?unspecified have to be filled out, as in
//   "Irving told Joe where the honey was" -- the "where" being represented
//   in the CD with an '?unspecified form.
define method doit (cd)
  let newcd
      = if (header-cd(cd) = #"mtrans"
             & knows(cdpath(#(#"actor"), cd), cdpath(#(#"object"), cd)))
          setrole(#"object",
                  knows(cdpath(#(#"actor"), cd), cdpath(#(#"object"), cd)),
                  cd);
        else
          cd;
        end if;
  assert-fact(newcd);
  newcd;
end method doit;

//   assert-fact is one of the central control functions.  It starts with
//   one fact, infers the consequences, infers the consequences of the
//   consequences, etc.  Besides the simple result put in *conseqs*
//   (e.g., ptrans changes locs), new states may lead to response actions
//   (put in *actions*) or new plans (put in *plans*).  The plans are
//   done after all the consequences are inferred.
define method assert-fact (x)
  *actions* := #f;
  *plans* := #f;
  forward-chain(list(x));
  begin
    do(method (cd) doit(setrole(#"time", *default-tense*, cd)); end method,
       *actions*);
    *actions*;
  end;
  begin
    do(// LTD: Function EVAL not yet implemented.
       eval, *plans*);
    *plans*;
  end;
end method assert-fact;

define method forward-chain (l)
  *conseqs* := #f;
  begin
    do(method (i) now-knows(#"world", i, #f); conseqs(i); end method, l);
    l;
  end;
  if (*conseqs*) forward-chain(*conseqs*); end if;
end method forward-chain;

//   Each act and state is associated with a function for 
//   calculating the consequences.
define method conseqs (cd)
  select (header-cd(cd))
    #"atrans"
       => atrans-conseqs(cd);
    #"grasp"
       => grasp-conseqs(cd);
    #"ingest"
       => ingest-conseqs(cd);
    #"loc"
       => loc-conseqs(cd);
    #"mbuild"
       => mbuild-conseqs(cd);
    #"mloc"
       => mloc-conseqs(cd);
    #"mtrans"
       => mtrans-conseqs(cd);
    #"plan"
       => plan-conseqs(cd);
    #"propel"
       => propel-conseqs(cd);
    #"ptrans"
       => ptrans-conseqs(cd);
    otherwise
       => #f;
  end select;
end method conseqs;

//   add-conseq adds and returns a CD to the list of consequences
define method add-conseq (x) push!(x, *conseqs*); x; end method add-conseq;

//   Consequences of an atrans: everyone in the area notices it and the
//   resulting change of possesion, the object changes locations, and the
//   from filler knows he no longer has it.
define method atrans-conseqs (cd)
  notice(cdpath(#(#"actor"), cd), cd);
  notice(cdpath(#(#"actor"), cd),
         add-conseq(has(cdpath(#(#"to"), cd), cdpath(#(#"object"), cd))));
  add-conseq(is-at(cdpath(#(#"object"), cd), cdpath(#(#"to"), cd)));
  if (cdpath(#(#"from"), cd))
    notice(cdpath(#(#"actor"), cd),
           add-conseq(negate(has(cdpath(#(#"from"), cd),
                                 cdpath(#(#"object"), cd)))));
  end if;
end method atrans-conseqs;

//   Consequences of a grasp: everyone knows that the actor either has or
//   (in the case of a tf (transition final or the end of an action) of the
//   grasp)  doesn't have the object
define method grasp-conseqs (cd)
  notice(cdpath(#(#"actor"), cd),
         add-conseq(if (in-mode(cd, #"tf"))
                      negate(has(cdpath(#(#"actor"), cd),
                                 cdpath(#(#"object"), cd)));
                    else
                      has(cdpath(#(#"actor"), cd), cdpath(#(#"object"), cd));
                    end if));
end method grasp-conseqs;

//   Consequences of an ingest: everyone knows that the actor 
//   is no longer hungry or thirsty.
define method ingest-conseqs (cd)
  notice(cdpath(#(#"actor"), cd),
         add-conseq(state(cdpath(#(#"actor"), cd),
                          if (cdpath(#(#"object"), cd) = #"water")
                            #"thirsty";
                          else
                            #"hungry";
                          end if,
                          #"neg")));
end method ingest-conseqs;

//   Consequences of a loc change: everyone knows it.
define method loc-conseqs (cd)
  notice(cdpath(#(#"actor"), cd), cd);
end method loc-conseqs;

//   Consequences of an mbuild: if the object is a causal then a demon
//   is set up for the actor that will be triggered by the antecedent.
define method mbuild-conseqs (cd)
  if (cdpath(#(#"actor"), cd) = cdpath(#(#"object", #"conseq", #"actor"), cd))
    put(cdpath(#(#"actor"), cd), #"demons",
        pair(pair(cdpath(#(#"object", #"ante"), cd),
                  cdpath(#(#"object", #"conseq"), cd)),
             symbol-get-property(cdpath(#(#"actor"), cd), #"demons")));
  end if;
  #f;
end method mbuild-conseqs;

//   Consequences of an mloc change: check the demons to see if the
//   learned fact affects the learner.  Also check the reaction list
//   for general responses to learning such facts.
define method mloc-conseqs (cd)
  demon-check(cdpath(#(#"val", #"part"), cd), cdpath(#(#"con"), cd));
  if (~ member?(#"neg", cdpath(#(#"con", #"mode"), cd)))
    select (header-cd(cdpath(#(#"con"), cd)))
      #"loc"
         => loc-react(cd);
      #"mloc"
         => mloc-react(cd);
      #"hungry"
         => hunger-react(cd);
      #"thirsty"
         => thirst-react(cd);
      otherwise
         => #f;
    end select;
  end if;
end method mloc-conseqs;

//   Stored under each character is a list of "demons."  A demon is
//   a CD pattern plus an action.  Whenever the character learns
//   something this list is checked to see if there is a response to make.
//   Demons are set up by things like the mbuild in a bargain-plan.
define method demon-check (who, event)
  put(who, #"demons",
      choose(complement(empty?),
             begin
               let list92543 = symbol-get-property(who, #"demons");
               begin
                 do(method (demon)
                      if (unify-cds(head(demon), event))
                        push!(tail(demon), *actions*);
                        #f;
                      else
                        demon;
                      end if;
                    end method,
                    list92543);
                 list92543;
               end;
             end));
end method demon-check;

//   Consequences of an mtrans: if there is a ques in the CD mtransed,
//   and if it is a causal, then it is a bargaining promise; otherwise,
//   it is a request (assuming the actors in the sub-CD are in the right
//   places).  If there is no ques in the CD mtransed, then the hearer
//   knows about the mtrans, and if he believes the speaker, then he
//   believes what the speaker believes.
define method mtrans-conseqs (cd)
  let actor = cdpath(#(#"actor"), cd);
  let object = cdpath(#(#"object"), cd);
  let hearer = cdpath(#(#"to", #"part"), cd);
  if (member?(#"ques", cdpath(#(#"object", #"mode"), cd)))
    if (header-cd(object) = #"cause"
         & actor = cdpath(#(#"object", #"ante", #"actor"), cd)
         & hearer = cdpath(#(#"object", #"conseq", #"actor"), cd))
      promise-conseqs(hearer, cdpath(#(#"object", #"conseq"), cd), actor,
                      cdpath(#(#"object", #"ante"), cd));
    elseif (cdpath(#(#"object", #"actor"), cd) = hearer)
      request-conseqs(actor, hearer, future(un-question(object)));
    end if;
  elseif (~ (actor = hearer))
    add-conseq(mloc(hearer, cd));
    if (~ relate(hearer, actor, hearer, #"deceive"))
      add-conseq(mloc(hearer, mloc(actor, object)));
    elseif (nil);
    end if;
  end if;
end method mtrans-conseqs;

//   Consequences of y asking x to promise to do xdo if y does ydo:
//   If x deceives y, then after ydo, x will call y stupid, but says
//   that he will do xdo in return for ydo;
//   else if x likes y, then x will do xdo after ydo and says so.
//   Otherwise x says no.
define method promise-conseqs (x, xdo, y, ydo)
  let a = cause(ydo, affirm(xdo));
  if (relate(x, x, y, #"deceive"))
    add-conseq(mbuild(x,
                      cause(ydo,
                            future(mtrans(x,
                                          state(y, #"smart", #"neg"),
                                          y,
                                          x)))));
    add-conseq(mtrans(x, a, y, x));
  elseif (relate(x, x, y, #"like"))
    add-conseq(mbuild(x, a));
    add-conseq(mtrans(x, a, y, x));
  else
    add-conseq(mtrans(x, negate(a), y, x));
  end if;
end method promise-conseqs;

//   Consequences of x asking y to do z: 
//   If y doesn't like x or dominates x, then y will say no; otherwise
//   y will do z.
define method request-conseqs (x, y, z)
  add-conseq(if (~ relate(y, y, x, #"like") | relate(y, y, x, #"dominate"))
               plan(y, future(mtrans(y, negate(z), x, y)));
             else
               plan(y, z);
             end if);
end method request-conseqs;

//   Consequences of a plan: If the actor of the plan act is the actor of 
//   the object of the plan, then add the object to the list of actions.
define method plan-conseqs (cd)
  if (cdpath(#(#"actor"), cd) = cdpath(#(#"object", #"actor"), cd))
    push!(cdpath(#(#"object"), cd), *actions*);
  end if;
  #f;
end method plan-conseqs;

//   Consequences of a propel: the object struck dies
define method propel-conseqs (cd)
  if (member?(cdpath(#(#"to"), cd), *personae*))
    add-conseq(state(cdpath(#(#"to"), cd), #"health", #"neg"));
  end if;
end method propel-conseqs;

//   Consequences of a ptrans: location change, for both actor
//   and object.
define method ptrans-conseqs (cd)
  add-conseq(is-at(cdpath(#(#"object"), cd), cdpath(#(#"to"), cd)));
  if (~ (cdpath(#(#"actor"), cd) = cdpath(#(#"object"), cd)))
    add-conseq(is-at(cdpath(#(#"actor"), cd), cdpath(#(#"to"), cd)));
  end if;
end method ptrans-conseqs;

//   Reactions to learning of a location change: if it's food or water,
//   check to see if learner is hungry or thirsty.
define method loc-react (cd)
  member?(cdpath(#(#"con", #"actor"), cd),
          get-isa(#"food", cdpath(#(#"val", #"part"), cd)))
   | cdpath(#(#"con", #"actor"), cd) = #"water"
   & sgoal-check(cdpath(#(#"val", #"part"), cd),
                 if (cdpath(#(#"con", #"actor"), cd) = #"water")
                   #"thirsty";
                 else
                   #"hungry";
                 end if);
end method loc-react;

//   If a character is hungry or thirsty, add the appropriate s-goal
//   to the list of plans.
define method sgoal-check (actor, scale)
  in-state(actor, scale)
   & push!(list(if (scale = #"thirsty") #"sthirst"; else #"shunger"; end if,
                list(#"quote", actor)),
           *plans*);
end method sgoal-check;

//   Reactions to learning that someone has learned something:
//   if it's someone else, and it's about himself or you believe he
//   doesn't deceive you, then you believe it too.
define method mloc-react (cd)
  ~ (cdpath(#(#"val", #"part"), cd) = cdpath(#(#"con", #"val", #"part"), cd))
   & (cdpath(#(#"con", #"con", #"actor"), cd)
       = cdpath(#(#"con", #"val", #"part"), cd)
       | ~ relate(cdpath(#(#"val", #"part"), cd),
                  cdpath(#(#"con", #"val", #"part"), cd),
                  cdpath(#(#"val", #"part"), cd), #"deceive"))
   & add-conseq(mloc(cdpath(#(#"val", #"part"), cd),
                     cdpath(#(#"con", #"con"), cd)));
end method mloc-react;

//   Reactions to learning that you're hungry: add s-goal to list
//   of plans.
define method hunger-react (cd)
  push!(list(#"shunger", list(#"quote", cdpath(#(#"con", #"actor"), cd))),
        *plans*);
end method hunger-react;

//   Reactions to learning you're thirsty: add s-goal to list 
//   of plans.
define method thirst-react (cd)
  push!(list(#"sthirst", list(#"quote", cdpath(#(#"con", #"actor"), cd))),
        *plans*);
end method thirst-react;

//   Notice says that everyone in the same location as who knows
//   about CD.
define method notice (who, cd)
  let where = loc-name-of(who);
  begin
    do(method (persona)
         if (loc(persona) = where) add-conseq(mloc(persona, cd)); end if;
       end method,
       *personae*);
    *personae*;
  end;
end method notice;

//   Memory functions and pattern matcher
//   addfact adds a CD to knower's knowledge set.  Also if world
//   learns a character has died, then the character is removed from the
//   global list of characters.
//   The CD is added to the front of the fact list, so that memquery
//   will get the most recent CD that matches its query pattern.  Older
//   contradicted facts are still on the list but are not seen.
define method addfact (knower, cd)
  put(knower, #"facts", pair(cd, symbol-get-property(knower, #"facts")));
  //  Now check for deceased people.
  if (knower = #"world" & header-cd(cd) = #"health"
       & member?(#"neg", cdpath(#(#"mode"), cd)))
    *personae* := remove(*personae*, cdpath(#(#"actor"), cd));
  end if;
  #f;
end method addfact;

//   is-state returns non-nil if CD is one of the state forms.
define method is-state (cd)
  member?(header-cd(cd),
          #(#"loc", #"mloc", #"cont", #"like", #"deceive", #"dominate",
            #"hungry", #"thristy", #"health", #"smart"));
end method is-state;

//   now-knows adds what to the data base for who.  It also prints in
//   English this new fact.  If who = world (a true fact) and what is
//   an mloc, then save the content of the mloc under the person who
//   learned it.  If say-flag is t, then mlocs are always generated in
//   English; otherwise only facts (who = world) are generated.  This
//   reduces the volume of the output.
define method now-knows (who, what, say-flag)
  let newwho
      = if (who = #"world" & header-cd(what) = #"mloc")
          cdpath(#(#"val", #"part"), what);
        else
          who;
        end if;
  let newwhat
      = if (who = #"world" & header-cd(what) = #"mloc")
          cdpath(#(#"con"), what);
        else
          what;
        end if;
  if (say-flag | newwho = #"world") say(mloc(newwho, newwhat)); end if;
  addfact(newwho, newwhat);
end method now-knows;

//   knows(knower,fact) returns fact if fact is in data base for knower:
//   -- if fact = knows(knower,subfact), assume everyone knows what they
//      know and look up subfact,
//   -- if fact has a ?unspec, then return the filler that replaces
//     the ?unspec in the data base.
define method knows (knower, fact)
  let newfact
      = if (header-cd(fact) = #"mloc"
             & cdpath(#(#"val", #"part"), fact) = knower)
          cdpath(#(#"con"), fact);
        else
          fact;
        end if;
  memquery(knower, newfact);
end method knows;

define method knows-loc (knower, object)
  cdpath(#(#"val"), knows(knower, where-is(object)));
end method knows-loc;

define method knows-owner (knower, object)
  cdpath(#(#"val"), knows(knower, who-has(object)));
end method knows-owner;

define method knows-if (knower, cd)
  cdpath(#(#"mode"), knows(knower, setrole(#"mode", #"?unspecified", cd)));
end method knows-if;

//   memquery find the first item in knower's data base that
//   matches fact.
define method memquery (knower, pat)
  head(pat-member(pat, symbol-get-property(knower, #"facts")));
end method memquery;

//   pat-member finds the first item in cd-list that matches
//   pat and returns cd-list from that item on.
define method pat-member (pat, cd-list)
  if (cd-list)
    let cd = head(cd-list);
    if (unify-cds(pat, cd))
      cd-list;
    else
      pat-member(pat, tail(cd-list));
    end if;
  end if;
end method pat-member;

//   Returns non-nil if actor has goal.
define method has-goal-of (actor, pat)
  head(pat-member(pat, symbol-get-property(actor, #"goals")));
end method has-goal-of;

//   Adds goal to data base.
define method gets-new-goal-of (actor, goal)
  put(actor, #"goals", pair(goal, symbol-get-property(actor, #"goals")));
  say(wants(actor, goal));
end method gets-new-goal-of;

//   Removes goal from data base
define method forgets-goal-of (actor, goal)
  let goal-to-be-forgotten = has-goal-of(actor, goal);
  put(actor, #"goals",
      choose(complement(method (g) g = goal-to-be-forgotten; end method),
             symbol-get-property(actor, #"goals")));
end method forgets-goal-of;

//   Returns non-nil if x is in a state, e.g., hungry.
define method in-state (x, st)
  find-out(#"world", state(x, st, #"pos"));
end method in-state;

//   Returns non-nil if X believes that y relates to z in a certain way.
//   Usually either y or z is x.
define method relate (x, y, z, rel)
  find-out(x, relation(y, z, rel, #"pos"));
end method relate;

//   Looks up CD in the data base for who.  If there, return non-nil if
//   the CD is not a negative fact.  If not there, ask the user at the
//   terminal and save the result.  Note that the generator is used to
//   ask questions.
// 
//   find-out is used to determine if a given character is in a
//   given state (e.g., is the character hungry or thirsty) and is
//   also used to determine how two characters relate to on another
//   (e.g., do they like one another?, does one have a tendency to
//   deceive the other, etc.).
define method find-out (who, cd)
  let mode = knows-if(who, cd);
  if (mode)
    member?(#"pos", mode);
  else
    say(mloc(who, cd));
    format-out("\n [Y/N]? \n>");
    begin
      let answer
          = // LTD: Function READ not yet implemented.
            read()
             = #"y";
      addfact(who,
              setrole(#"mode", list(if (answer) #"pos"; else #"neg"; end if),
                      cd));
      answer;
    end;
  end if;
end method find-out;

//   True if y thinks x is a friend of his.
define method is-friend-of (x, y)
  ~ (x = y) & relate(y, x, y, #"like");
end method is-friend-of;

//   Returns location of x.
define method loc (x) knows-loc(#"world", x); end method loc;

//   True if x and y are in the same place.
define method is-prox (x, y)
  loc-name-of(x) = loc-name-of(y);
end method is-prox;

//   A CD is true if it's an mloc and the content is in the person's
//   data base, or it's in the data base for world.
define method is-true (cd)
  if (header-cd(cd) = #"mloc")
    knows(cdpath(#(#"val", #"part"), cd), cdpath(#(#"con"), cd));
  else
    knows(#"world", cd);
  end if;
end method is-true;

//   loc-name-of returns the real location of x.  This may involve going
//   up several levels -- e.g., when Joe takes a worm, its location is
//   stored as joe, but its real location is the location Joe is at.
define method loc-name-of (x)
  let loc-of-x = loc(x);
  if (member?(x, *all-locations*))
    x;
  elseif (member?(loc-of-x, *all-locations*))
    loc-of-x;
    //  If something isn't anywhere in particular, 
    //  then it on the ground.
    elseif (empty?(loc-of-x))
    #"ground";
  else
    loc-name-of(loc-of-x);
  end if;
end method loc-name-of;

//   get-isa is like get but checks is-a node for x if x has no
//   y property.				
define method get-isa (x, y)
  symbol-get-property(y, x)
   | symbol-get-property(symbol-get-property(y, #"is-a"), x);
end method get-isa;

//   Functions to build CD forms
//   Acts
define method atrans (actor, object, to, from)
  list(#"atrans", list(#"actor", actor), list(#"object", object),
       list(#"to", to), list(#"from", from));
end method atrans;

define method cause (x, y)
  list(#"cause", list(#"ante", x), list(#"conseq", y));
end method cause;

define method grasp (actor, object)
  list(#"grasp", list(#"actor", actor), list(#"object", object));
end method grasp;

define method un-grasp (actor, object)
  tf(grasp(actor, object));
end method un-grasp;

define method ingest (actor, object)
  list(#"ingest", list(#"actor", actor), list(#"object", object));
end method ingest;

define method mbuild (actor, object)
  list(#"mbuild", list(#"actor", actor), list(#"object", object));
end method mbuild;

define method mtrans (actor, object, to, from)
  list(#"mtrans", list(#"actor", actor), list(#"object", object),
       list(#"to", list(#"cp", list(#"part", to))), list(#"from", from));
end method mtrans;

define method plan (actor, object)
  list(#"plan", list(#"actor", actor), list(#"object", object));
end method plan;

define method propel (actor, object, to)
  list(#"propel", list(#"actor", actor), list(#"object", object),
       list(#"to", to));
end method propel;

define method ptrans (actor, object, to, from)
  if (to)
    list(#"ptrans", list(#"actor", actor), list(#"object", object),
         list(#"to", to), list(#"from", from));
  end if;
end method ptrans;

define method wants (actor, goal)
  list(#"want", list(#"actor", actor), list(#"object", goal));
end method wants;

//   States
define method has (actor, object)
  list(#"cont", list(#"actor", object), list(#"val", actor));
end method has;

define method is-at (actor, loc)
  list(#"loc", list(#"actor", actor), list(#"val", loc));
end method is-at;

define method mloc (actor, con)
  list(#"mloc", list(#"con", con),
       list(#"val", list(#"cp", list(#"part", actor))));
end method mloc;

define method state (actor, st, mode)
  list(st, list(#"actor", actor), list(#"mode", list(mode)));
end method state;

define method relation (actor, object, rel, mode)
  list(rel, list(#"actor", actor), list(#"to", object),
       list(#"mode", list(mode)));
end method relation;

define method where-is (x)
  list(#"loc", list(#"actor", x), list(#"val", #"?unspecified"));
end method where-is;

define method who-has (x)
  list(#"cont", list(#"actor", x), list(#"val", #"?unspecified"));
end method who-has;

//   Mode functions
define method mode (cd) cdpath(#(#"mode"), cd); end method mode;

//   Affirm/Negate set the mode of a CD to true/false.
define method affirm (cd)
  if (member?(#"pos", mode(cd)))
    cd;
  else
    setrole(#"mode", pair(#"pos", remove(mode(cd), #"neg")), cd);
  end if;
end method affirm;

define method negate (cd)
  if (member?(#"neg", mode(cd)))
    affirm(cd);
  else
    setrole(#"mode", pair(#"neg", remove(mode(cd), #"pos")), cd);
  end if;
end method negate;

//   maybe makes a CD hypothetical -- doesn't matter if it's true or false.
define method maybe (cd)
  if (member?(#"maybe", mode(cd)))
    cd;
  else
    setrole(#"mode", pair(#"maybe", mode(cd)), cd);
  end if;
end method maybe;

//   question/un-question make a CD a question/non-question -- doesn't
//   matter if it's true or false.
define method question (cd)
  if (member?(#"ques", mode(cd)))
    cd;
  else
    setrole(#"mode", pair(#"ques", mode(cd)), cd);
  end if;
end method question;

define method un-question (cd)
  setrole(#"mode", remove(mode(cd), #"ques"), cd);
end method un-question;

//   tf adds "transition final" to a CD -- doesn't matter if it's true
//   or false.
define method tf (cd)
  if (member?(#"tf", mode(cd)))
    cd;
  else
    setrole(#"mode", pair(#"tf", mode(cd)), cd);
  end if;
end method tf;

//   future sets a CD to a future time.
define method future (cd) setrole(#"time", #"future", cd); end method future;

//   Path
// 
//   cdpath finds the filler at the end of the role list in a CD.
// 
//   For example, if
//   CD = (mtrans (actor joe)
//                (object (ptrans (actor joe) 
//                                (object worm)
//                                (from joe)
//                                (to irving))))
//   then
//   (cdpath '(actor) cd) returns joe;
//   (cdpath '(object) cd) returns (ptrans (actor joe) 
//                                         (object worm)
//                                         (from joe)
//                                         (to irving));
//   (cdpath '(object object) cd) returns worm.
// 
//   If a role doesn't exist in a CD form, then cdpath returns nil.
define method cdpath (rolelist, cd)
  if (empty?(rolelist))
    cd;
  else
    cdpath(tail(rolelist), filler-role(head(rolelist), cd));
  end if;
end method cdpath;

//   micro-mumble: micro English generator
//   say prints a CD as an English sentence.  If CD is an mloc of the
//   world, then only the fact itself is said, otherwise the whole mloc
//   is used.  The original CD is returned.  say1 is called with the 
//   infinitive flag off and the say-subject flag on.
define method say (cd)
  let cd-to-be-said
      = if (unify-cds(#(#"mloc", #(#"val", #(#"cp", #(#"part", #"world")))),
                      cd))
          cdpath(#(#"con"), cd);
        else
          cd;
        end if;
  format-out("\n");
  say1(cd-to-be-said, cdpath(#(#"time"), cd-to-be-said) | *default-tense*, #f,
       #t);
  format-out(".");
  cd;
end method say;

//   say1 prints cd according to the program under the head predicate.
//   If no program is there, the CD is printed with <>s around it.
//   
//   These generation programs are lists of expressions to be evaluated.
//   Attached to primative acts, they are normally concerned with
//   generating subject-verb-object clauses.  Since some of the acts,
//   such as mtrans, want and plan, take subclauses, the generator has to
//   be recursive, so that the atrans program that generates the clause
//   "Joe gave Irving the worm" can also generate the subclause in
//   "Joe planned to give Irving the worm." This means that the programs have
//   to know when to say or not say the subject, when to use the 
//   infinitive form, and what tense to use.
//     subj = true means print the subject,
//     inf = true means use the infinitive form,
//     tense is set to either past, present, or future, or cond (for
//             conditional, i.e., hypothetical)
define method say1 (cd, tense, inf, subj)
  let say-fun = symbol-get-property(header-cd(cd), #"say-fun");
  if (say-fun)
    apply(say-fun, list(cd, tense, inf, subj));
  else
    format-out("\n < %= > ", cd);
  end if;
end method say1;

//   subclause recursively calls say1 with the subconcept at the 
//   endpoint of rolelist.  word, if non-nil, starts the subclause,
//   unless relative-pronoun has a better idea.  Tense is calculated 
//   by sub-tense.
define method subclause (cd, word, rolelist, tense)
  if (word) format-out(" %= ", relative-pronoun(rolelist, cd) | word); end if;
  let subcd = cdpath(rolelist, cd);
  say1(subcd, sub-tense(tense, subcd), #f, #t);
end method subclause;

//   sub-tense is given a tense and a CD and picks the tense to use.
//   The given tense is used, except with states (i.e., don't
//   say "he told him where the honey would be" even though conceptually
//   that's right), and with past statements about the future (i.e., say
//   "he said he would" rather than "he said he will").
define method sub-tense (tense, subcd)
  if (is-state(subcd))
    *default-tense*;
  elseif (tense = #"past" & cdpath(#(#"time"), subcd) = #"future")
    #"cond";
  else
    tense;
  end if;
end method sub-tense;

//   relative-pronoun returns the word to start the subclause
//   for the CD at the end of the CD role path.
define method relative-pronoun (rolelist, cd)
  let subcd = cdpath(rolelist, cd);
  if (header-cd(subcd) = #"loc" & pcvar-p(cdpath(#(#"val"), subcd)))
    #"where";
  elseif (pcvar-p(next-subject(cd)))
    #"who";
  else
    #f;
  end if;
end method relative-pronoun;

//   next-subject returns the subject of a subconcept, which is normally
//   the actor slot, except for cont (where it's in the val slot) and
//   mloc (where it's in the part slot of the val slot).
define method next-subject (cd)
  let subcd = cdpath(#(#"object"), cd);
  cdpath(select (header-cd(subcd))
           #"cont"
              => #(#"val");
           #"mloc"
              => #(#"val", #"part");
           otherwise
              => #(#"actor");
         end select,
         subcd);
end method next-subject;

//   infclause calls recursively say1 with the subconcept at the
//   endpoint of rolelist.  An infinitive is printed, and the subject
//   is suppressed.
define method infclause (cd, rolelist, subj-flag, tense)
  say1(cdpath(rolelist, cd), tense, #t, subj-flag);
end method infclause;

//   Store say-funs for each of the CD forms
//   atrans may go to either "take" (if actor = to) or "give."
define method say-atrans (cd, tense, inf, subj)
  if (cdpath(#(#"actor"), cd) = cdpath(#(#"to"), cd))
    say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"take");
    say-filler(cd, #(#"object"));
    say-prep(cd, #"from", #(#"from"));
  else
    say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"give");
    say-filler(cd, #(#"to"));
    say-filler(cd, #(#"object"));
  end if;
end method say-atrans;

put(#"atrans", #"say-fun", say-atrans);

//   mtrans may go to either "ask whether" or "tell that"
define method say-mtrans (cd, tense, inf, subj)
  if (member?(#"ques", cdpath(#(#"object", #"mode"), cd)))
    say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"ask");
    say-filler(cd, #(#"to", #"part"));
    subclause(cd, #"whether", #(#"object"), #"cond");
  else
    say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"tell");
    say-filler(cd, #(#"to", #"part"));
    subclause(cd, #"that", #(#"object"), cdpath(#(#"time"), cd));
  end if;
end method say-mtrans;

put(#"mtrans", #"say-fun", say-mtrans);

//   ptrans may go to either "go" or "move."
define method say-ptrans (cd, tense, inf, subj)
  if (cdpath(#(#"actor"), cd) = cdpath(#(#"object"), cd))
    say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"go");
  else
    say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"move");
    say-filler(cd, #(#"object"));
  end if;
  say-prep(cd, #"to", #(#"to"));
end method say-ptrans;

put(#"ptrans", #"say-fun", say-ptrans);

//   mbuild may go to either "decide to" or "decide that."
define method say-mbuild (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"decide");
  if (cdpath(#(#"actor"), cd) = cdpath(#(#"object", #"actor"), cd))
    infclause(cd, #(#"object"), #f, #"future");
  else
    subclause(cd, #"that", #(#"object"), #"future");
  end if;
end method say-mbuild;

put(#"mbuild", #"say-fun", say-mbuild);

//   propel goes to strike
define method say-propel (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"strike");
  say-filler(cd, #(#"to"));
end method say-propel;

put(#"propel", #"say-fun", say-propel);

//   grasp may go to either "let go of" or "grab."
define method say-grasp (cd, tense, inf, subj)
  if (in-mode(cd, #"tf"))
    say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"let");
    format-out(" GO OF  ");
  else
    say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"grab");
  end if;
  say-filler(cd, #(#"object"));
end method say-grasp;

put(#"grasp", #"say-fun", say-grasp);

//   ingest may go to either "eat" or "drink."
define method say-ingest (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"),
                if (cdpath(#(#"object"), cd) = #"water")
                  #"drink";
                else
                  #"eat";
                end if);
  say-filler(cd, #(#"object"));
end method say-ingest;

put(#"ingest", #"say-fun", say-ingest);

//   plan goes to "plan."
define method say-plan (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"plan");
  infclause(cd, #(#"object"), #f, #"future");
end method say-plan;

put(#"plan", #"say-fun", say-plan);

//   want goes to "want to" -- the third argument of infclause is set to 
//   true if the subject of the subclause is different that the subject
//   of the main clause.
define method say-want (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"want");
  infclause(cd, #(#"object"), ~ (cdpath(#(#"actor"), cd) = next-subject(cd)),
            #"future");
end method say-want;

put(#"want", #"say-fun", say-want);

//   loc goes to "be near."
define method say-loc (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"be");
  pcvar-p(cdpath(#(#"val"), cd)) | say-prep(cd, #"near", #(#"val"));
end method say-loc;

put(#"loc", #"say-fun", say-loc);

//   cont goes to "have."
define method say-cont (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"val"), #"have");
  say-filler(cd, #(#"actor"));
end method say-cont;

put(#"cont", #"say-fun", say-cont);

//   mloc may go to either "know that", "know whether", or "think that."
define method say-mloc (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"val", #"part"),
                if (relative-pronoun(#(#"con"), cd)
                     | is-true(cdpath(#(#"con"), cd)))
                  #"know";
                else
                  #"think";
                end if);
  subclause(cd, #"that", #(#"con"), *default-tense*);
end method say-mloc;

put(#"mloc", #"say-fun", say-mloc);

//   health goes to "be alive"
define method say-health (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"be");
  format-out(" ALIVE ");
end method say-health;

put(#"health", #"say-fun", say-health);

//   smart goes to "be bright"
define method say-smart (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"be");
  format-out(" BRIGHT ");
end method say-smart;

put(#"smart", #"say-fun", say-smart);

//   hungry goes to "be hungry"
define method say-hungry (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"be");
  format-out(" HUNGRY ");
end method say-hungry;

put(#"hungry", #"say-fun", say-hungry);

//   thirsty goes to "be thirsty"
define method say-thirsty (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"be");
  format-out(" THIRSTY ");
end method say-thirsty;

put(#"thirsty", #"say-fun", say-thirsty);

//   cause may go to either "x if y" or "if x then y"
define method say-cause (cd, tense, inf, subj)
  if (in-mode(cd, #"ques"))
    subclause(cd, #f, #(#"conseq"), #"future");
    format-out(" IF ");
    subclause(cd, #f, #(#"ante"),
              select (tense)
                #"figure"
                   => #"present";
                #"cond"
                   => *default-tense*;
                otherwise
                   => tense;
              end select);
  else
    format-out(" IF ");
    subclause(cd, #f, #(#"ante"), #"future");
    format-out(" THEN ");
    subclause(cd, #f, #(#"conseq"), #"cond");
  end if;
end method say-cause;

put(#"cause", #"say-fun", say-cause);

//   like goes to "like"
define method say-like (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"like");
  say-filler(cd, #(#"to"));
end method say-like;

put(#"like", #"say-fun", say-like);

//   dominate goes to "dominate"
define method say-dominate (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"dominate");
  say-filler(cd, #(#"to"));
end method say-dominate;

put(#"dominate", #"say-fun", say-dominate);

//   deceive goes to "deceive"
define method say-deceive (cd, tense, inf, subj)
  say-subj-verb(cd, tense, inf, subj, #(#"actor"), #"deceive");
  say-filler(cd, #(#"to"));
end method say-deceive;

put(#"deceive", #"say-fun", say-deceive);

//   say-filler prints the CD at the end of a CD role path
define method say-filler (cd, rolelist)
  say-pp(cdpath(rolelist, cd));
end method say-filler;

//   say-pp prints a CD (adds "the" to object).
define method say-pp (cd)
  if (member?(cd, *all-objects*)) format-out(" THE "); end if;
  format-out("%=", cd);
end method say-pp;

//   say-prep prints a preposition plus a CD at the end of a role path,
//   if any exists.
define method say-prep (cd, prep, rolelist)
  let subcd = cdpath(rolelist, cd);
  if (subcd) format-out(" %= ", prep); say-pp(subcd); elseif (nil); end if;
end method say-prep;

//   in-mode tests whether x is in CD's mode.
define method in-mode (cd, x)
  member?(x, cdpath(#(#"mode"), cd));
end method in-mode;

//   say-neg prints "not" if CD is negative.
define method say-neg (cd)
  if (in-mode(cd, #"neg")) format-out(" NOT"); end if;
end method say-neg;

//   say-subj-verb prints the subject (unless suppressed by
//   subj = nil, infinitives, or an ?unspec as the subject) and verb, 
//   with auxillary and tensing, if any.  Note that future tense is 
//   treated as an auxillary.
define method say-subj-verb (cd, tense, inf, subj, rolelist, infinitive)
  let subject = cdpath(rolelist, cd);
  if (inf)
    if (subj) say-pp(subject); end if;
    say-neg(cd);
    format-out(" TO %= ", infinitive);
  else
    if (~ pcvar-p(subject)) say-pp(subject); end if;
    begin
      let plural = symbol-get-property(subject, #"plural");
      let auxilary
          = if (in-mode(cd, #"maybe"))
              #"might";
            elseif (tense = #"future")
              if (*default-tense* = #"past") #"would"; else #"will"; end if;
            elseif (tense = #"cond")
              #"would";
            elseif (in-mode(cd, #"neg") & ~ (infinitive = #"be"))
              #"do";
            end if;
      if (auxilary)
        say-tense(cd, tense, inf, subj, auxilary, plural);
        format-out(" ");
        say-neg(cd);
        format-out(" %= ", infinitive);
      else
        say-tense(cd, tense, inf, subj, infinitive, plural);
        format-out(" ");
        if (infinitive = #"be") say-neg(cd); end if;
      end if;
    end;
  end if;
end method say-subj-verb;

//   say-tense prints a verb, with tense and number inflection.
//   Conjugations of irregular verbs are stored under the past and present
//   properties of the verb, in the format (singular plural) for each.
//   For regular verbs, say-tense adds "d", "ed", or "s" as appropriate.
define method say-tense (cd, tense, inf, subj, infinitive, plural)
  let tense-forms = symbol-get-property(infinitive, tense);
  format-out(" ");
  if (tense-forms)
    format-out("%=",
               if (plural)
                 second(tense-forms);
               else
                 head(tense-forms);
               end if);
  else
    format-out("%=", infinitive);
    select (tense)
      #"past"
         => if (~ (lastchar(infinitive) = 'E' | lastchar(infinitive) = 'e'))
              format-out("E");
            end if;
             format-out("D ");
      #"present"
         => if (~ plural) format-out("S "); end if;
      otherwise
         => #f;
    end select;
  end if;
end method say-tense;

//   lastchar returns that last character in x
define method lastchar (x)
  head(begin
         let s93739 = explode(x);
         copy-sequence(s93739, start: size(s93739) - 1);
       end);
end method lastchar;

define method explode (x)
  as(<list>,
     // LTD: Function PRINC-TO-STRING not yet implemented.
     princ-to-string(x));
end method explode;

//   Generator Dictionary
// 
//   Set the past and/or present tenses for irregular verbs.
//   Each tense is of the form (singular plural).
put(#"be", #"past", #(#"was", #"were"));

put(#"be", #"present", #(#"is", #"are"));

put(#"do", #"past", #(#"did", #"did"));

put(#"do", #"present", #(#"does", #"do"));

put(#"drink", #"past", #(#"drank", #"drank"));

put(#"eat", #"past", #(#"ate", #"ate"));

put(#"give", #"past", #(#"gave", #"gave"));

put(#"go", #"past", #(#"went", #"went"));

put(#"go", #"present", #(#"goes", #"go"));

put(#"grab", #"past", #(#"grabbed", #"grabbed"));

put(#"have", #"past", #(#"had", #"had"));

put(#"have", #"present", #(#"has", #"have"));

put(#"know", #"past", #(#"knew", #"knew"));

put(#"let", #"past", #(#"let", #"let"));

put(#"might", #"past", #(#"might", #"might"));

put(#"might", #"present", #(#"might", #"might"));

put(#"plan", #"past", #(#"planned", #"planned"));

put(#"strike", #"past", #(#"struck", #"struck"));

put(#"take", #"past", #(#"took", #"took"));

put(#"tell", #"past", #(#"told", #"told"));

put(#"think", #"past", #(#"thought", #"thought"));

//   Berries is the only plural in the current set-up.
put(#"berries", #"plural", #t);

//   CD Functions
//   is-cd-p determines whether a given sexpr is a CD.
define method is-cd-p (x)
  instance?(x, <list>) & not(instance?(header-cd(x), <list>))
   & list-of-role-filler-pairs-p(roles-cd(x));
end method is-cd-p;

define method list-of-role-filler-pairs-p (x)
  empty?(x)
   | (instance?(x, <list>) & instance?(head(x), <list>)
       & not(instance?(role-pair(head(x)), <list>))
       & list-of-role-filler-pairs-p(tail(x)));
end method list-of-role-filler-pairs-p;

//   header-cd gets the head act of a CD form.
define method header-cd (x) head(x); end method header-cd;

//   roles-cd gets the list of role-pairs of a CD form.
define method roles-cd (x) tail(x); end method roles-cd;

//   Role-pairs have the form (role filler).
//   role-pair returns the role.
define method role-pair (x) head(x); end method role-pair;

//   filler-pair returns the filler.
define method filler-pair (x) second(x); end method filler-pair;

//   A filler for a role is found by looking for the role name in the CD,
//   and returning the filler if a pair is found.
define method filler-role (role, cd)
  if (instance?(cd, <list>))
    let pair = cl-assoc(role, roles-cd(cd));
    if (pair) filler-pair(pair); end if;
  end if;
end method filler-role;

//   setrole makes a new CD form with (role filler) added
//   or replacing the old (role ...) pair.
define method setrole (role, filler, cd)
  pair(header-cd(cd),
       pair(list(role, filler),
            choose(complement(method (pair) head(pair) == role; end method),
                   roles-cd(cd))));
end method setrole;

//   Pattern Unifier
//   This unifier is an adapted version of the unify function which appears
//   in the book _Artificial_Intelligence_Programming_ (2nd ed.)
//   Eugene Chaniak, Drew McDermott, and James Meehan.
define method unify (pat1, pat2) unify-1(pat1, pat2, #f); end method unify;

define method unify-1 (pat1, pat2, sub)
  if (pcvar-p(pat1))
    var-unify(pat1, pat2, sub);
  elseif (pcvar-p(pat2))
    var-unify(pat2, pat1, sub);
  elseif (not(instance?(pat1, <list>)))
    if (pat1 == pat2) list(sub); else #f; end if;
  elseif (not(instance?(pat2, <list>)))
    #f;
  else
    apply(concatenate!,
          map(method (sub) unify-1(tail(pat1), tail(pat2), sub); end method,
              unify-1(head(pat1), head(pat2), sub)));
  end if;
end method unify-1;

define variable *occurscheck-p* = #t;

define method var-unify (pcvar, pat, sub)
  if (pcvar == pat | (pcvar-p(pat) & pcvar.pcvar-id == pat.pcvar-id))
    list(sub);
  else
    begin
      let binding = pcvar-binding(pcvar, sub);
      if (binding)
        unify-1(binding-value(binding), pat, sub);
      elseif (*occurscheck-p* & occurs-in-p(pcvar, pat, sub))
        #f;
      else
        list(extend-binding(pcvar, pat, sub));
      end if;
    end;
  end if;
end method var-unify;

define method occurs-in-p (pcvar, pat, sub)
  if (pcvar-p(pat))
    pcvar.pcvar-id == pat.pcvar-id
     | begin
         let binding = pcvar-binding(pat, sub);
         (binding & occurs-in-p(pcvar, binding-value(binding), sub));
       end;
  elseif (not(instance?(pat, <list>)))
    #f;
  else
    occurs-in-p(pcvar, head(pat), sub) | occurs-in-p(pcvar, tail(pat), sub);
  end if;
end method occurs-in-p;

define method pcvar-binding (pcvar, alist)
  cl-assoc(pcvar.pcvar-id, alist);
end method pcvar-binding;

define method extend-binding (pcvar, pat, alist)
  pair(list(pcvar.pcvar-id, pat), alist);
end method extend-binding;

define method binding-value (binding) second(binding); end method binding-value;

define method pcvar-value (pat, sub)
  let binding = pcvar-binding(pat, sub);
  if (empty?(binding))
    pat;
  else
    begin
      let value = binding-value(binding);
      if (value == pat) pat; else replace-variables(value, sub); end if;
    end;
  end if;
end method pcvar-value;

define method replace-variables (pat, sub)
  if (pcvar-p(pat))
    pcvar-value(pat, sub);
  elseif (not(instance?(pat, <list>)))
    pat;
  else
    pair(replace-variables(head(pat), sub),
         replace-variables(tail(pat), sub));
  end if;
end method replace-variables;

define method instantiate (pat, subs)
  if (pcvar-p(pat))
    begin
      let entry = cl-assoc(pat.pcvar-id, subs);
      if (entry) instantiate(second(entry), subs); else pat; end if;
    end;
  elseif (not(instance?(pat, <list>)))
    pat;
  else
    pair(instantiate(head(pat), subs), instantiate(tail(pat), subs));
  end if;
end method instantiate;

//   CD Unifier
//   This replaces the less-general CD pattern matcher that was
//   used in the original Micro-Talespin program.  This unifier
//   allows pattern variables to appear on both of the
//   expressions to be compared while a pattern matcher
//   only allows variables to appear in one of the expressions.
define method unify-cds (cd1, cd2)
  unify-cds-1(cd1, cd2, #f);
end method unify-cds;

define method unify-cds-1 (cd1, cd2, sub)
  header-cd(cd1) == header-cd(cd2)
   & unify-pairs(roles-cd(cd1), roles-cd(cd2), sub);
end method unify-cds-1;

//   unify-pairs sees if the roles and fillers of a CD can
//   be matched together.  It is more complicated than the
//   function unify-1 given above because (1) the role-filler pairs
//   do not need to be in the same order in the two CDs being
//   compared; (2) a missing pair in one CD means that that CD
//   is more general than the other CD and can, thus, be matched
//   against it; and, finally, (3) the filler of a pair can be a CD,
//   and most fillers which are lists are CDs, however, fillers which
//   are "modes" are the exception; they are fillers which are lists,
//   but are not CDs, so a special exception has to be made for them
//   in the unification procedure below.
define method unify-pairs (pairs1, pairs2, sub)
  if (empty?(pairs1) | empty?(pairs2))
    list(sub);
  else
    let role = role-pair(head(pairs1));
    let pair-from-pairs2 = cl-assoc(role, pairs2);
    let rest-of-pairs-from-pairs2
        = choose(complement(method (pair) role-pair(pair) = role; end method),
                 pairs2);
    let newsubs
        = if (role == #"mode")
            unify-1(head(pairs1), pair-from-pairs2, sub);
          elseif (pair-from-pairs2
                   & (pcvar-p(second(pair-from-pairs2))
                       | not(instance?(second(pair-from-pairs2), <list>))))
            unify-1(head(pairs1), pair-from-pairs2, sub);
          elseif (pair-from-pairs2
                   & (pcvar-p(second(head(pairs1)))
                       | not(instance?(second(head(pairs1)), <list>))))
            unify-1(head(pairs1), pair-from-pairs2, sub);
          elseif (pair-from-pairs2)
            unify-cds-1(head(pairs1), pair-from-pairs2, sub);
          else
            list(sub);
          end if;
    apply(concatenate!,
          map(method (newsub)
                unify-pairs(tail(pairs1), rest-of-pairs-from-pairs2, newsub);
              end method,
              newsubs));
  end if;
end method unify-pairs;

//   Done loading
format-out("\nDone loading Micro-Talespin");

