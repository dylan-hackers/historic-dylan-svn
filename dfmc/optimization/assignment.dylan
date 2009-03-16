Module:   dfmc-optimization
Synopsis: Assignment elimination & (in the future) optimization.
Author:   Jonathan Bachrach, Keith Playford, Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Eliminate-assignments turns all temporaries that have assignments
// into boxed objects, and replaces all references to those temporaries
// to primitive operations which work on these boxed objects.

// define compilation-pass eliminate-assignments,
//   visit: functions,
//   mandatory?: #t,
//   before: analyze-calls;

define method eliminate-assignments (f :: <&lambda>)
  for (t in f.environment.temporaries)
    if (~empty?(t.assignments) & ~cell?(t))
      cell-assigned-temporaries(t);
    end if;
  end for;
  strip-assignments(environment(f));
end method eliminate-assignments;

define method cell-assigned-temporaries (t :: <temporary>)
  let (make-cell-first-c, make-cell-last-c, cell) 
    = convert-make-cell(t.environment, t);
  insert-computations-after!
    (t.generator | t.environment.lambda.body, 
     make-cell-first-c, make-cell-last-c);
  for (user in t.users)
    unless (user == make-cell-first-c | user == make-cell-last-c)
      let (get-first-c, get-last-c, get-t)
        = with-parent-computation (user)
            convert-get-cell-value(user.environment, cell);
          end;
      insert-computations-before-reference!
        (user, get-first-c, get-last-c, t);
      replace-temporary-references!(user, t, get-t);
    end unless;
  end for;
  for (assignment in t.assignments)
    assert(assignment.assigned-binding == t);
    let val-t = assignment.computation-value;
    let (set-first-c, set-c, set-t)
      = with-parent-computation (assignment)
          convert-set-cell-value!(assignment.environment, cell, val-t);
        end;
    insert-computations-after!(assignment, set-first-c, set-c);
    replace-temporary-in-users!(assignment.temporary, val-t);
    delete-computation!(assignment);
    // Track cell writes
    cell.assignments := add!(cell.assignments, set-c); 
  end for;
  t.assignments := #(); // should this happen automatically?
end method cell-assigned-temporaries;

// Constructors for celling primitives.

/// TODO: SHOULD RESTRICT RAW CELLS TO WHEN THEY DONT CREATE MORE
///       BOX/UNBOX THAN THEY REMOVE... 

define method convert-make-cell 
    (env :: <lambda-lexical-environment>, t :: <temporary>)
 => (first-c :: <computation>, last-c :: <computation>, t :: <cell>);
   with-parent-computation (t.generator)
     let type = specializer(t);
     if (type == dylan-value(#"<object>"))
       //XXX: one day, dylan-value(#"<object>") should be gone entirely
       type := make(<&top-type>);
     end;
     let (unboxer-c, unboxed-t)
       = maybe-convert-unbox(env, t, type);
     let (c, tmp) 
       = make-with-temporary
	   (env, <make-cell>, value: unboxed-t, temporary-class: <cell>);
     let cell = c.temporary;
     cell-type(cell) := type;
     rename-temporary!(t, cell);
     join-1x1-t!(unboxer-c, c, tmp);
   end;
end method convert-make-cell;

define method convert-get-cell-value 
    (env :: <lambda-lexical-environment>, cell :: <cell>)
 => (first-c :: <computation>, last-c :: <computation>, t :: <temporary>)
  let (c, tmp) =
    make-with-temporary(env, <get-cell-value>, cell: cell);
  let (boxer-c, boxed-t)
    = maybe-convert-box(env, tmp, cell-type(cell));
  join-1x1-t!(c, boxer-c, boxed-t);
end method convert-get-cell-value;

define method convert-set-cell-value!
    (env :: <lambda-lexical-environment>, cell :: <cell>, 
     ref :: <value-reference>)
 => (first-c :: <computation>, last-c :: <computation>, t :: <temporary>)
  let (unboxer-c, unboxed-t)
    = maybe-convert-unbox(env, ref, cell-type(cell));
  let (c, tmp) =
    make-with-temporary
      (env, <set-cell-value!>, cell: cell, value: unboxed-t);
  join-1x1-t!(unboxer-c, c, tmp);
end method convert-set-cell-value!;

// eof
