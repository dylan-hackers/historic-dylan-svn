//  Copyright 1994, Brown University, Providence, RI
//  See end of file for full copyright information
//  (in-package 'user)
//  Least commitment planning
//  In the following, a plan consists of a set of steps corresponding to
//  operations, constraints on the order in which these steps are to be
//  carried out, and information regarding how the steps depend on one
//  another.  The dependency information is of three different types.
//  First, there are records called requirements indicating propositions
//  that are required in order that certain operators have their desired
//  effect. Next, there are records called links describing how the
//  consequences of one operator are used to satisfy the requirements of
//  another. Finally, there are records called conflicts describing
//  potential undesirable interactions between operators. As an example of
//  a conflict, one operator might delete a proposition that another
//  operator adds, where the deleted proposition is required for the plan
//  to succeed.
define method make-plan (steps, constraints, conflicts, links, requirements)
  list(steps, constraints, conflicts, links, requirements);
end method make-plan;

define method plan-steps (plan) head(plan); end method plan-steps;

define method plan-constraints (plan) second(plan); end method plan-constraints;

define method plan-conflicts (plan) third(plan); end method plan-conflicts;

define method plan-links (plan) cadddr(plan); end method plan-links;

define method plan-requirements (plan)
  cadddr(tail(plan));
end method plan-requirements;

//  A link involves two steps and a condition. One step, called the
//  producer, has the condition in the list of additions for its
//  associated operator.  The producer makes the condition true. The other
//  step, called the consumer, has the condition in the list of
//  preconditions for its associated operator.  The consumer requires that
//  the condition be true. Links are created to satisfy requirements.
define method make-link (producer, condition, consumer)
  list(producer, condition, consumer);
end method make-link;

define method link-producer (link) head(link); end method link-producer;

define method link-condition (link) second(link); end method link-condition;

define method link-consumer (link) third(link); end method link-consumer;

//  A conflict involves a link and a step. The step, called the clobberer,
//  has the condition of the link in the list of deletions for its
//  associated operator. A conflict arises when a step is added to a plan
//  and that step might be carried out after the producer of a link and
//  before the consumer, preventing the link from satisfying the
//  requirement that the link was introduced to satisfy. When a conflict
//  occurs the clobberer is said to threaten link.
define method make-conflict (link, clobberer)
  list(link, clobberer);
end method make-conflict;

define method conflict-link (conflict) head(conflict); end method conflict-link;

define method conflict-clobberer (conflict)
  second(conflict);
end method conflict-clobberer;

//  We represent constraints in terms of their begin and end steps, and 
//  assign each step a unique integer to distinguish different steps 
//  employing the same operator.
begin
  let n = 0;
  define method make-step (operator)
    list(n := n + 1, operator);
  end method make-step;
end;

define method step-id (step) head(step); end method step-id;

define method step-operator (step) second(step); end method step-operator;

//  As in previous section, we describe a state as a set of conditions.
//  A requirement corresponds to a step and a condition that must be true
//  immediately prior to that step. The condition of the requirement
//  corresponds to a precondition of the operator indicated in the step.
define method make-requirement (step, condition)
  list(step, condition);
end method make-requirement;

define method requirement-step (req) head(req); end method requirement-step;

define method requirement-condition (req)
  second(req);
end method requirement-condition;

define method make-operator (preconditions, additions, deletions)
  list(preconditions, additions, deletions);
end method make-operator;

define method preconditions (operator) head(operator); end method preconditions;

define method additions (operator) head(tail(operator)); end method additions;

define method deletions (operator)
  head(tail(tail(operator)));
end method deletions;

//  The Lisp implementation for refinements is provided as follows. Recall
//  that, unless the plan satisfies the goal, it has either conflicts or
//  requirements.
define method refinements (plan, operators)
  if (~ empty?(plan-conflicts(plan)))
    resolve-conflict(head(plan-conflicts(plan)), plan);
  else
    concatenate(resolve-req-new-step(head(plan-requirements(plan)), operators,
                                     plan),
                resolve-req-existing-step(head(plan-requirements(plan)),
                                          plan));
  end if;
end method refinements;

//  Conflicts are resolved by constraining the clobberer to occur before
//  the producer of the associated link or after the consumer of the link.
define method resolve-conflict (conflict, plan)
  let link = conflict-link(conflict);
  let step = conflict-clobberer(conflict);
  concatenate(constrain(step, link-producer(link), plan),
              constrain(link-consumer(link), step, plan));
end method resolve-conflict;

//  A new plan is created with one step to occur before another if the two
//  steps are not already constrained to occur in the opposite order.
define method constrain (step1, step2, plan)
  if (precedes(step2, step1, plan-constraints(plan)))
    #f;
  else
    list(make-plan(plan-steps(plan),
                   add!(list(step1, step2), plan-constraints(plan), test: \=),
                   tail(plan-conflicts(plan)), plan-links(plan),
                   plan-requirements(plan)));
  end if;
end method constrain;

//  Precedes determines if one step precedes another given a set of 
//  constraints.
define method precedes (step1, step2, constraints)
  step1 = step2
   | any?(method (c)
            (step1 = head(c) & precedes(second(c), step2, constraints));
          end method,
          constraints);
end method precedes;

//  To eliminate a requirement by adding a new step, create a new plan for
//  each applicable operator.
define method resolve-req-new-step (req, operators, plan)
  apply(concatenate!,
        map(method (p) applicablep(p, req, plan); end method, operators));
end method resolve-req-new-step;

//  An operator is applicable just in case the condition of the
//  requirement is added by the operator. If applicable, create a new plan
//  from the old one by adding a new step, constraining the new step to
//  precede the step of the resolved requirement, elimiating this
//  requirement, adding a link resolving the requirement with the new step
//  as producer, adding a new set of requirements corresponding to the
//  preconditions of the operator, and updating the set of conflicts.
//  Conflicts can arise when the deletions of the new step threaten
//  existing links or when existing steps threaten the new link.
define method applicablep (operator, req, plan)
  if (~ member?(requirement-condition(req), additions(operator), test: \=))
    #f;
  else
    let step = make-step(operator);
    let constraint = list(step, requirement-step(req));
    let link
        = make-link(step, requirement-condition(req), requirement-step(req));
    list(make-plan(pair(step, plan-steps(plan)),
                   add!(constraint, plan-constraints(plan), test: \=),
                   concatenate(link-conflicts(link, plan),
                               step-conflicts(step, plan),
                               plan-conflicts(plan)),
                   pair(link, plan-links(plan)),
                   concatenate(generate-requirements(operator, step),
                               tail(plan-requirements(plan)))));
  end if;
end method applicablep;

define method resolve-req-existing-step (req, plan)
  apply(concatenate!,
        map(method (s) linkablep(s, req, plan); end method,
            plan-steps(plan)));
end method resolve-req-existing-step;

//  An existing step can be linked to satisfy a requirement just in case
//  its associated operator adds the condition of the requirement and the
//  existing step is not constrained to follow the step of the
//  requirement. If this criterion is met, create a new plan from the old
//  one by constraining the existing step to the step of the resolved
//  requirement, eliminating this requirement, adding a link resolving the
//  requirement with the existing step as producer, and updating the set
//  of conflicts.
define method linkablep (step, req, plan)
  if (~ member?(requirement-condition(req), additions(step-operator(step)),
                test: \=)
       | precedes(requirement-step(req), step, plan-constraints(plan)))
    #f;
  else
    let link
        = make-link(step, requirement-condition(req), requirement-step(req));
    let constraint = list(step, requirement-step(req));
    list(make-plan(plan-steps(plan),
                   add!(constraint, plan-constraints(plan), test: \=),
                   concatenate(link-conflicts(link, plan),
                               plan-conflicts(plan)),
                   pair(link, plan-links(plan)),
                   tail(plan-requirements(plan))));
  end if;
end method linkablep;

//  When a link is added, we find all steps that might conflict with it.
//  This function extends the data abstraction for objects of type LINK.
define method link-conflicts (link, plan)
  apply(concatenate!,
        map(method (step) conflictp(link, step); end method,
            plan-steps(plan)));
end method link-conflicts;

//  When a step is added, we find all links that might conflict with it.
//  This function extends the data abstraction for objects of type STEP.
define method step-conflicts (step, plan)
  apply(concatenate!,
        map(method (link) conflictp(link, step); end method,
            plan-links(plan)));
end method step-conflicts;

//  A link and a step conflict whenever the operator of the step deletes 
//  the condition of the link, unless the step is the consumer of the 
//  link.
define method conflictp (link, step)
  if (~ (step = link-consumer(link))
       & member?(link-condition(link), deletions(step-operator(step)),
                 test: \=))
    list(make-conflict(link, step));
  else
    list();
  end if;
end method conflictp;

//  The operator of a step has associated with it one requirement for each
//  of its preconditions.
define method generate-requirements (operator, step)
  map(method (p) make-requirement(step, p); end method,
      preconditions(operator));
end method generate-requirements;

define method best (states, goalp, next, comparator)
  if (empty?(states))
    #f;
  elseif (goalp(head(states)))
    head(states);
  else
    best(sort!(concatenate(next(head(states)), tail(states)),
               test: comparator),
         goalp, next, comparator);
  end if;
end method best;

//  Simple abstract test:
//  (setq start (make-step (make-operator () '(P Q) ())))
//  (setq finish (make-step (make-operator '(R) () ())))
//  (setq reqs (list (make-requirement finish 'R)))
//  (setq operators (list (make-operator '(P) '(R) ())
//  		      (make-operator '(Q) '(S) ())))
//  (setq plan (make-plan (list start finish)
//  		      (list (list start finish)) () () 
//  		      reqs))
//  Sussman's anomaly:
//  The start step uses a pseudo operator to encode the initial 
//  conditions.
start
 := make-step(make-operator(#(),
                            #(#(#"on", #"c", #"a"), #(#"on", #"a", #"table"),
                              #(#"on", #"b", #"table"), #(#"clear", #"c"),
                              #(#"clear", #"b")),
                            #()));

//  The finish step uses a pseudo operator to encode the goal conditions
finish
 := make-step(make-operator(#(#(#"on", #"a", #"b"), #(#"on", #"b", #"c")),
                            #(), #()));

//  The initial requirements correspond to the goal conditions
requirements
 := list(make-requirement(finish, #(#"on", #"a", #"b")),
         make-requirement(finish, #(#"on", #"b", #"c")));

//  The initial plan steps consists of the start and the finish steps.
plan
 := make-plan(list(start, finish), list(list(start, finish)), #(), #(),
              requirements);

//  We show only the operators needed for the example.
operators
 := list(make-operator(#(#(#"on", #"c", #"a"), #(#"clear", #"c")),
                       #(#(#"on", #"c", #"table"), #(#"clear", #"a")),
                       #(#(#"on", #"c", #"a"))),
         make-operator(#(#(#"on", #"a", #"table"), #(#"clear", #"a"),
                         #(#"clear", #"b")),
                       #(#(#"on", #"a", #"b")),
                       #(#(#"on", #"a", #"table"), #(#"clear", #"b"))),
         make-operator(#(#(#"on", #"b", #"table"), #(#"clear", #"b"),
                         #(#"clear", #"c")),
                       #(#(#"on", #"b", #"c")),
                       #(#(#"on", #"b", #"table"), #(#"clear", #"c"))));

//  Test function:
define method test ()
  if (best(list(plan),
           method (p)
             if (empty?(plan-conflicts(p)) & empty?(plan-requirements(p)))
               plan-print(p);
               #t;
             else
               #f;
             end if;
           end method,
           method (p) refinements(p, operators); end method,
           method (p, q)
             size(plan-requirements(p)) < size(plan-requirements(q));
           end method))
    #t;
  else
    #f;
  end if;
end method test;

define method partial (plan)
  map(method (step1)
        pair(step-id(step1),
             apply(concatenate!,
                   map(method (step2)
                         ~ precedes(step1, step2, plan-constraints(plan))
                          & ~ precedes(step2, step1, plan-constraints(plan))
                          & list(step-id(step2));
                       end method,
                       plan-steps(plan))));
      end method,
      sort!(plan-steps(plan),
            test: method (x, y)
                    precedes(x, y, plan-constraints(plan));
                  end method));
end method partial;

//  Pretty printer for plans:
define method plan-print (p)
  *print-pretty* := #t;
  write-element(*standard-output*, '\n');
  print("Steps:", *standard-output*);
  write-element(*standard-output*, '\n');
  for (steps = plan-steps(p) then cdr(steps), until empty?(steps))
    print(head(steps), *standard-output*);
    write-element(*standard-output*, '\n');
  finally
    #f;
  end for;
  print("Constraints:", *standard-output*);
  write-element(*standard-output*, '\n');
  for (constraints = plan-constraints(p) then cdr(constraints),
       until empty?(constraints))
    print(step-id(head(head(constraints))), *standard-output*);
    print(" precedes ", *standard-output*);
    print(step-id(second(head(constraints))), *standard-output*);
    write-element(*standard-output*, '\n');
  finally
    #f;
  end for;
  print("Links:", *standard-output*);
  write-element(*standard-output*, '\n');
  for (links = plan-links(p) then cdr(links), until empty?(links))
    link-print(head(links));
    write-element(*standard-output*, '\n');
  finally
    #f;
  end for;
  print("Conflicts:", *standard-output*);
  write-element(*standard-output*, '\n');
  for (conflicts = plan-conflicts(p) then cdr(conflicts),
       until empty?(conflicts))
    print(step-id(conflict-clobberer(head(conflicts))), *standard-output*);
    print(" conflicts with ", *standard-output*);
    link-print(conflict-link(head(conflicts)));
    write-element(*standard-output*, '\n');
  finally
    #f;
  end for;
  print("Requirements:", *standard-output*);
  write-element(*standard-output*, '\n');
  for (reqs = plan-requirements(p) then cdr(reqs), until empty?(reqs))
    print(step-id(requirement-step(head(reqs))), *standard-output*);
    print(" requires ", *standard-output*);
    print(requirement-condition(head(reqs)), *standard-output*);
    write-element(*standard-output*, '\n');
  finally
    #f;
  end for;
  print("Partial order:", *standard-output*);
  write-element(*standard-output*, '\n');
  print(partial(p), *standard-output*);
end method plan-print;

define method link-print (link)
  print(step-id(link-producer(link)), *standard-output*);
  print(" produces ", *standard-output*);
  print(link-condition(link), *standard-output*);
  print(" for ", *standard-output*);
  print(step-id(link-consumer(link)), *standard-output*);
end method link-print;

//  > (test)
//  
//  Steps:
//  (5 (((ON B TABLE) (CLEAR B) (CLEAR C)) 
//      ((ON B C))
//      ((ON B TABLE) (CLEAR C))))                                   
//  (4 (((ON C A) (CLEAR C)) 
//      ((ON C TABLE) (CLEAR A))
//      ((ON C A))))
//  (3 (((ON A TABLE) (CLEAR A) (CLEAR B)) 
//      ((ON A B))
//      ((ON A TABLE) (CLEAR B))))
//  (2 (((ON A B) (ON B C)) 
//      NIL 
//      NIL))
//  (1 (NIL 
//      ((ON C A) (ON A TABLE) (ON B TABLE) (CLEAR C) (CLEAR B))
//      NIL))
//  Constraints:
//  5 precedes 3
//  1 precedes 5
//  4 precedes 5
//  5 precedes 2
//  1 precedes 4
//  4 precedes 3
//  1 precedes 3
//  3 precedes 2
//  1 precedes 2
//  Links:
//  1 produces (ON B TABLE) for 5
//  1 produces (CLEAR B) for 5
//  1 produces (CLEAR C) for 5
//  5 produces (ON B C) for 2
//  1 produces (ON A TABLE) for 3
//  1 produces (ON C A) for 4
//  1 produces (CLEAR C) for 4
//  4 produces (CLEAR A) for 3
//  1 produces (CLEAR B) for 3
//  3 produces (ON A B) for 2
//  Conflicts:
//  Requirements:
//  Copyright 1994, Brown University, Providence, RI
//  Permission to use and modify this software and its documentation
//  for any purpose other than its incorporation into a commercial
//  product is hereby granted without fee.  Permission to copy and
//  distribute this software and its documentation only for
//  non-commercial use is also granted without fee, provided, however
//  that the above copyright notice appear in all copies, that both
//  that copyright notice and this permission notice appear in
//  supporting documentation, that the name of Brown University not
//  be used in advertising or publicity pertaining to distribution
//  of the software without specific, written prior permission, and
//  that the person doing the distribution notify Brown University
//  of such distributions outside of his or her organization. Brown
//  University makes no representations about the suitability of this
//  software for any purpose. It is provided "as is" without express
//  or implied warranty.  Brown University requests notification of
//  any modifications to this software or its documentation.
// 
//  Send the following redistribution information
// 
//  	Name:
//  	Organization:
//  	Address (postal and/or electronic):
// 
//  To:
//  	Software Librarian
//  	Computer Science Department, Box 1910
//  	Brown University
//  	Providence, RI 02912
// 
//  		or
// 
//  	brusd@cs.brown.edu
// 
//  We will acknowledge all electronic notifications.
"eof";

