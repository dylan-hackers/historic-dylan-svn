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
  let res = next-method();
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

define method succ (c :: <block>) => (res :: <comp-vector>)
  let res = next-method();
  add!(res, c.body);
  res;
end;

define method succ (c :: <unwind-protect>) => (res :: <comp-vector>)
  let res = next-method();
  add!(res, c.cleanups);
  res;
end;

define method dominators (r :: <computation>) => (dominators :: <table>, ass :: <comp-vector>)
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
  let ass = make(<comp-vector>);

  local method dfs (v :: <computation>)
          n := n + 1;
          semi[v] := n;
          vertex[n] := v;
          label[v] := v;
          ancestor[v] := 0;
          //child[v] := 0;
          //dsize[v] := 1;
          if (instance?(v, <assignment>) & ~instance?(v, <definition>) & ~member?(v, ass))
            add!(ass, v);
          end;
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
  values(dom, ass);
end;

// (Efficiently computing single static assignment and the control dependence graph
//  Cytron, Ferrante, Kosen, Wegman, Zadeck, 1991)

define class <tree-node> (<object>)
  slot node-computation :: <computation>, required-init-keyword: computation:;
  slot node-children :: <stretchy-vector> = make(<stretchy-vector>), init-keyword: children:;
end;

define function post-order (r :: <tree-node>, f :: <function>)
  do(rcurry(post-order, f), r.node-children);
  f(r);
end;

define function build-tree (t :: <table>) => (root :: <tree-node>, mapping :: <table>)
  let mapping = make(<table>);
  let root = #f;
  local method get-tree-node (c :: <computation>)
          if (element(mapping, c, default: #f))
            mapping[c];
          else
            let n = make(<tree-node>, computation: c);
            mapping[c] := n;
          end;
        end;
  for (node in t.key-sequence)
    let idom = t[node];
    let n = get-tree-node(node);
    if (idom == 0)
      root := n;
    else
      add!(idom.get-tree-node.node-children, n);
    end;
  end;
  values(root, mapping);
end;

define function dominance-frontier (idom :: <table>, root :: <tree-node>)
 => (dominance-frontier :: <table>)
  let df = make(<table>);
  local method visit(tree-node :: <tree-node>)
          let x = tree-node.node-computation;
          df[x] := make(<comp-vector>);
          for (y in succ(x))
            if (idom[y] ~= x)
              df[x] := add!(df[x], y);
            end;
          end;
          for (z in node-children(tree-node))
            for (y in df[z.node-computation])
              if (idom[y] ~= x)
                df[x] := add!(df[x], y);
              end;
            end;
          end;
        end;
  post-order(root, visit);
  df;
end;

define method phi-placement (df :: <table>, ass :: <comp-vector>, mapping :: <table>)
  let itercount = 0;
  let hasalready = make(<table>);
  let work = make(<table>);
  let w = make(<deque>);
  for (x in ass)
    if (~ instance?(x.assigned-binding, <module-binding>))
      let v = x.assigned-binding;
      itercount := itercount + 1;
      work[x] := itercount;
      push(w, x);
      while (~ w.empty?)
        let x = w.pop;
        for (y in df[x])
          if (element(hasalready, y, default: 0) < itercount)
            //place-phi-for-v-at-y
            let (phi, tmp)
              = make-with-temporary
                  (y.environment, <phi-node>, variable: v);
            if (instance?(y, <loop>))
              insert-computations-before!(y.loop-body, phi, phi);
            else
              insert-computations-after!(y, phi, phi);
            end;
            v.assignments := add!(v.assignments, phi);

            //incremental updates of dominator tree
            let prev-dom = mapping[y];
            let dom-node
              = make(<tree-node>,
                     computation: phi,
                     children: prev-dom.node-children);
            prev-dom.node-children := make(<stretchy-vector>);
            add!(prev-dom.node-children, dom-node);            
            mapping[phi] := dom-node;

            //format-out("placed phi for %= at %=\n", v, y);
            hasalready[y] := itercount;
            if (element(work, y, default: 0) < itercount)
              work[y] := itercount;
              push(w, y);
            end;
          end;
        end;
      end;
    end;
  end;
end;

define function replace-temporaries! (c :: <computation>, vars :: <collection>, currvars :: <table>)
  for (accessors :: <temporary-accessors> in c.used-temporary-accessors)
    let getter = temporary-getter(accessors);
    let setter = temporary-zetter(accessors);
    let ref = getter(c);
    if (instance?(ref, <sequence>))
      for (ref-t in ref, index from 0)
        if (member?(ref-t, vars))
          let new-t = get-latest-assignment(ref-t, currvars);
          ref[index] := new-t;
          if (new-t) add-user!(new-t, c) end;
          remove-user!(ref-t, c);
	end if;
      end for;
    else
      if (member?(ref, vars))
        let new-t = get-latest-assignment(ref, currvars);
        setter(new-t, c);
        if (new-t) add-user!(new-t, c) end;
        remove-user!(ref, c);
      end if;
    end if;
  end for;
end;

define function get-latest-assignment (t :: <temporary>, vars :: <table>) => (res :: <temporary>)
  let result = element(vars, t, default: #f);
  assert(result ~= #f);
  let res = result.first;
  if (instance?(res, <computation>))
    res.temporary;
  else
    res;
  end;
end;

define function find-recent-assignments (phi :: <phi-node>, variable :: <lexical-variable>)
  local method find (c :: false-or(<computation>), m :: <function>)
          if (c)
            m(c) & c | find(c.previous-computation, m);
          else
            c
          end;
        end;
  let last-merge = find(phi, rcurry(instance?, type-union(<loop>, <binary-merge>)));
  local method get-last-ass (start :: <computation>)
          let last-ass = find(start, method(x)
                                       member?(x, variable.assignments) |
                                       (generator(variable) == x)
                                     end);
          if (last-ass)
            last-ass.temporary;
          elseif (variable.generator == #f)
            variable;
          end;
        end;
  let (left, right)
    = if (instance?(last-merge, <binary-merge>))
        values(last-merge.merge-right-previous-computation,
               last-merge.merge-left-previous-computation);
      elseif (instance?(last-merge, <loop>))
        let call = block(ret)
                     walk-computations(method(x)
                                         if (instance?(x, <loop-call>))
                                           ret(x);
                                         end
                                       end, last-merge.loop-body, #f)
                   end;
        values(last-merge.previous-computation, call);
      end;
  let last-left = left.get-last-ass;
  phi.merge-left-value := last-left;
  add-user!(last-left, phi);

  let last-right = right.get-last-ass;
  phi.merge-right-value := last-right;
  add-user!(last-right, phi);
end;

define function renaming (ass :: <comp-vector>, f :: <&lambda>, mapping :: <table>)
  let counter = make(<table>);
  let stacks = make(<table>);
  let modified-variables = make(<stretchy-vector>);
  for (assignment in ass)
    let v = assignment.assigned-binding;
    unless (instance?(v, <module-binding>))
      counter[v] := 0;
      stacks[v] := make(<deque>);
      unless (v.generator & v.environment == assignment.environment)
        //passed as argument, thus push v onto the stack
        push(stacks[v], v);
      end;
      add!(modified-variables, v);
    end;
  end;
  let assign = #();
  local method search (x :: <computation>)
          replace-temporaries!(x, modified-variables, stacks);
          let children = mapping[x].node-children;
          if (instance?(x, <assignment>) & ~ instance?(x, <definition>))
            unless (instance?(x.assigned-binding, <module-binding>))
              //replace assignment with <temporary-transfer>
              let (tt, tmp)
                = make-with-temporary
                    (x.environment, <temporary-transfer>, value: x.computation-value);
              insert-computations-after!(x, tt, tt);
              replace-temporary-in-users!(x.temporary, tmp);
              delete-computation!(x);

              //update assignment list
              let ass = x.assigned-binding.assignments;
              ass := remove!(ass, x);
              ass := add!(ass, tt);
              x.assigned-binding.assignments := ass;

              //update dominator-tree
              mapping[x].node-computation := tt;
              mapping[tt] := mapping[x];
              remove-key!(mapping, x);

              push(stacks[x.assigned-binding], tt);
            end;
          end;
          if (instance?(x, <phi-node>))
            push(stacks[x.phi-ssa-variable], x);
            find-recent-assignments(x, x.phi-ssa-variable);
          end;
          begin
            let v = x.temporary;
            if (member?(v, modified-variables))
              unless (member?(x, v.assignments) | v.generator == x)
                v.assignments := add!(v.assignments, x);
              end;
              push(stacks[v], x);
            end;
          end;
          //for (v in x.temporary)
          //  let i = counter[v];
          //  //replace v by vi in LHS
          //  //add to assignments of v [or not?]
          //  push(stacks[v], i);
          //  counter[v] := i + 1;
          //end;
          //for (y in succ(x))
          //  let j = which-predecessor(y, x);
          //  if (instance?(y, <phi-node>))
          //    //actually, we need to put the data-flow edges to the phi!
          //    //replace j-th operand in RHS(phi) by Vi where i = top(stacks[v])
          //  end;
          //end;
          for (y in children)
            search(y.node-computation);
          end;
          if (instance?(x, <assignment>) & ~ instance?(x, <definition>))
            unless (instance?(x.assigned-binding, <module-binding>))
              //for (v in oldLHS(x))
              pop(stacks[x.assigned-binding]);
              //end;
            end;
          end;
          if (instance?(x, <phi-node>))
            pop(stacks[x.phi-ssa-variable]);
          end;
          if (instance?(x, <temporary-transfer>))
            let v = x.temporary;
            if (instance?(v, <lexical-local-variable>))
              if (~empty?(v.assignments))
                pop(stacks[v]);
              end;
            end;
          end;
          if (instance?(x, <single-value-check-type-computation>))
            let v = x.temporary;
            if (member?(v, modified-variables))
              if (v.generator == x)
                pop(stacks[v]);
              end;
            end;
          end;
        end;
  if (modified-variables.size > 0)
    search(f.body);
  end;
end;


// Eliminate-assignments turns all temporaries that have assignments
// into boxed objects, and replaces all references to those temporaries
// to primitive operations which work on these boxed objects.

// define compilation-pass eliminate-assignments,
//   visit: functions,
//   mandatory?: #t,
//   before: analyze-calls;

define method eliminate-assignments (f :: <&lambda>)
  let (idom, ass) = dominators(f.body);
  let (root, mapping) = build-tree(idom);
  let df = dominance-frontier(idom, root);
  phi-placement(df, ass, mapping);
  renaming(ass, f, mapping);
  values(idom, root, mapping, df);
end method eliminate-assignments;

define method convert-ssa-to-cells (f :: <&lambda>)
  for (t in f.environment.temporaries)
    if (t & ~empty?(t.assignments) & ~cell?(t))
      cell-assigned-temporaries(t);
    end if;
  end for;
  strip-assignments(environment(f));
end;

define method cell-assigned-temporaries (t :: <temporary>)
  let (make-cell-first-c, make-cell-last-c, cell) 
    = convert-make-cell(t.environment, t);
  insert-computations-after!
    (t.generator | t.environment.lambda.body, 
     make-cell-first-c, make-cell-last-c);
  local method replace-user (user :: <computation>, replaced :: <temporary>)
          unless (user == make-cell-first-c | user == make-cell-last-c)
            let (get-first-c, get-last-c, get-t)
              = with-parent-computation (user)
                  convert-get-cell-value(user.environment, cell);
                end;
            insert-computations-before-reference!
              (user, get-first-c, get-last-c, replaced);
            replace-temporary-references!(user, replaced, get-t);
          end unless;
        end method;
  for (user in t.users)
    replace-user(user, t);
  end for;
  for (assignment in t.assignments)
    for (user in assignment.temporary.users)
      unless (instance?(user, <phi-node>))
        replace-user(user, assignment.temporary);
      end;
    end for;
    if (instance?(assignment, <phi-node>))
      delete-computation!(assignment);
    else //<temporary-transfer>
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
    end;
  end for;
end method cell-assigned-temporaries;

// Constructors for celling primitives.

/// TODO: SHOULD RESTRICT RAW CELLS TO WHEN THEY DONT CREATE MORE
///       BOX/UNBOX THAN THEY REMOVE... 

define method convert-make-cell 
    (env :: <lambda-lexical-environment>, t :: <temporary>)
 => (first-c :: <computation>, last-c :: <computation>, t :: <cell>);
   with-parent-computation (t.generator)
     let type = specializer(t); //or better, type-union of all assignments?
     if (~type | type == dylan-value(#"<object>"))
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
