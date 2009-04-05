Module:   dfmc-optimization
Synopsis: Assignment elimination & (in the future) optimization.
Author:   Jonathan Bachrach, Keith Playford, Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//eliminate assignments, convert CFG to SSA
// (A fast Algorithm for Finding Dominators in a Flowgraph - Lengauer, Tarjan, 1979)

define constant <comp-vector> = limited(<stretchy-vector>, of: <computation>);

define method succ (v :: <computation>) => (res :: <comp-vector>)
  let res = make(<comp-vector>);
  if (v.next-computation)
    add!(res, v.next-computation);
  end;
  res;
end;

define method succ (v :: <loop>) => (res :: <comp-vector>)
  let res = make(<comp-vector>);
  if (v.next-computation)
    add!(res, v.next-computation);
  end;
  add!(res, v.loop-body);
  res;
end;

define method succ (v :: <if>) => (res :: <comp-vector>)
  let res = make(<comp-vector>);
  add!(res, v.consequent);
  add!(res, v.alternative);
  res;  
end;

define method succ (v :: <loop-call>) => (res :: <comp-vector>)
  let res = make(<comp-vector>);
  add!(res, v.loop-call-loop);
  res;
end;

define method dominators (r :: <computation>) => (result :: <table>)
  let semi = make(<table>);
  let vertex = make(<table>);
  let ancestor = make(<table>);
  //let dsize = make(<table>);
  let label = make(<table>);
  let parent = make(<table>);
  let pred = make(<table>);
  //let child = make(<table>);
  let bucket = make(<table>);
  let dom = make(<table>);
  let n = 0;

  local method dfs (v :: <computation>)
          n := n + 1;
          semi[v] := n;
          vertex[n] := v;
          label[v] := v;
          ancestor[v] := 0;
          //child[v] := 0;
          //dsize[v] := 1;
          for (w in succ(v))
            unless (element(semi, w, default: #f))
              parent[w] := v;
              dfs(w);
            end;
            pred[w] := add!(element(pred, w, default: make(<comp-vector>)), v);
          end;
        end,
        method compress (v :: <computation>)
          if (ancestor[ancestor[v]] ~= 0)
            compress(ancestor[v]);
            if (semi[label[ancestor[v]]] < semi[label[v]])
              label[v] := label[ancestor[v]];
            end;
            ancestor[v] := ancestor[ancestor[v]];
          end;
        end, /*
        method eval (v :: <computation>)
          if (ancestor[v] == 0)
            label[v];
          else
            compress(v);
            if (semi[label[ancestor[v]]] >= semi[label[v]])
              label[v];
            else
              label[ancestor[v]];
            end;
          end;
        end,
        method link (v :: <computation>, w :: <computation>)
          let s = w;
          while (semi[label[w]] < semi[label[child[s]]])
            if (dsize[s] + dsize[child[child[s]]] >= 2 * dsize[child[s]])
              ancestor[child[s]] := s;
              child[s] := child[child[s]];
            else
              dsize[child[s]] := dsize[s];
              s := ancestor[s] := child[s];
            end;
          end;
          label[s] := label[w];
          dsize[v] := dsize[v] + dsize[w];
          if (dsize[v] < 2 * dsize[w])
            let tmp = s;
            s := child[v];
            child[v] := tmp;
            //s, child[v] := child[v], s
          end;
          while (s ~= 0)
            ancestor[s] := v;
            s := child[s];
          end;
        end; */
        method eval (v :: <computation>)
          if (ancestor[v] == 0)
            v
          else
            compress(v);
            label[v];
          end;
        end,
        method link (v :: <computation>, w :: <computation>)
          ancestor[w] := v;
        end;
  //for (v in succ(r))
  //  pred[v] := bucket[v] := make(<comp-vector>);
  //  semi[v] := 0;
  //end;
  //step 1
  dfs(r);
  //dsize[0] := label[0] := semi[v] := 0;
  for (i from n to 2 by -1)
    let w = vertex[i];
    //step 2
    for (v in pred[w])
      let u = eval(v);
      if (semi[u] < semi[w])
        semi[w] := semi[u];
      end;
    end;
    bucket[vertex[semi[w]]]
      := add!(element(bucket, vertex[semi[w]], default: make(<comp-vector>)),
              w);
    link(parent[w], w);
    //step 3
    for (v in copy-sequence(element(bucket, parent[w], default: make(<comp-vector>))))
      bucket[parent[w]] := remove!(bucket[parent[w]], v);
      let u = eval(v);
      dom[v] := if (semi[u] < semi[v])
                  u;
                else
                  parent[w];
                end;
    end;
  end;
  //step 4
  for (i from 2 below n)
    let w = vertex[i];
    if (dom[w] ~= vertex[semi[w]])
      dom[w] := dom[dom[w]];
    end;
  end;
  dom[r] := 0;
  dom;
end;

// (Efficiently computing single static assignment and the control dependence graph
//  Cytron, Ferrante, Kosen, Wegman, Zadeck, 1991)
/*
define method domain-frontier (dominator-tree)
  for (x in bottom-up-traversal(dom))
    df[x] := #();
    for (y in succ(x))
      if (idom(y) ~= x)
        df[x] := df[x] + {y};
      end;
    end;
    for (z in children(x))
      for (y in df[z])
        if (idom[y] ~= x)
          df[x] := df[x] + {y};
        end;
      end;
    end;
  end;
end;

define method phi-placement ()
  let itercount = 0;
  for (x in nodes)
    hasalready(x) := 0;
    work(x) := 0;
  end;
  let w = 0;
  for (v in variables)
    itercount := itercount + 1;
    for (x in a(v))
      work(x) := itercount;
      w := add!(w, x);
    end;
    while (~ w.empty?)
      let x = w.pop;
      for (y in df(x))
        if (hasalready(y) < itercount)
          place-phi-for-v-at-y
          hasalready(y) := itercount;
          if (work(y) < itercount)
            work(y) := itercount;
            w := add!(w, y);
          end;
        end;
      end;
    end;
  end;
end;
*/


// Eliminate-assignments turns all temporaries that have assignments
// into boxed objects, and replaces all references to those temporaries
// to primitive operations which work on these boxed objects.

// define compilation-pass eliminate-assignments,
//   visit: functions,
//   mandatory?: #t,
//   before: analyze-calls;

define method eliminate-assignments (f :: <&lambda>)
  dominators(f.body);
  for (t in f.environment.temporaries)
    if (~empty?(t.assignments) & ~cell?(t))
      cell-assigned-temporaries(t);
    end if;
  end for;
  strip-assignments(environment(f));
end method eliminate-assignments;

define method find-temporary-transfer (c :: <bind>, ass :: <collection>) => (res :: <computation>)
  c;
end;

define method find-temporary-transfer (c :: <computation>, ass :: <collection>) => (res :: <computation>)
  if (member?(c, ass))
    c
  else
    find-temporary-transfer(c.previous-computation, ass);
  end;
end;

define method cell-assigned-temporaries (t :: <temporary>)
  let ass = #();
  for (assignment in t.assignments)
    assert(assignment.assigned-binding == t);
    let (tt, tmp)
      = make-with-temporary
          (assignment.environment, <temporary-transfer>, value: assignment.computation-value);
    insert-computations-after!(assignment, tt, tt);
    replace-temporary-in-users!(assignment.temporary, tmp);
    delete-computation!(assignment);
    ass := add!(ass, tt);
  end;
  t.assignments := ass;
  for (user in t.users)
    //find nearest temporary-transfer
    let tt = find-temporary-transfer(user.previous-computation, ass);
    unless (instance?(tt, <bind>))
      replace-temporary-references!(user, t, tt.temporary);
    end;
  end;
               

 /*
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
  end for; */
end method cell-assigned-temporaries;

// Constructors for celling primitives.

/// TODO: SHOULD RESTRICT RAW CELLS TO WHEN THEY DONT CREATE MORE
///       BOX/UNBOX THAN THEY REMOVE... 

define method convert-make-cell 
    (env :: <lambda-lexical-environment>, t :: <temporary>)
 => (first-c :: <computation>, last-c :: <computation>, t :: <cell>);
   with-parent-computation (t.generator)
     let type = specializer(t);
     if (~type | type == dylan-value(#"<object>"))
       //XXX: one day, dylan-value(#"<object>") should be gone entirely
       //also, specializer slot of "a :: type-union(<foo>, <bar>)" is #f
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
