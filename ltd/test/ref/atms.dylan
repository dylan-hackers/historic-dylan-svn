//  -*- Mode: LISP; Syntax: Common-lisp; -*-
//  Assumption-based truth maintenance system, version 61 of 7/21/92.
//  Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
//  and Johan de Kleer, the Xerox Corporation.
//  All rights reserved.
//  See the file legal.txt for a paragraph stating scope of permission
//  and disclaimer of warranty.  The above copyright notice and that
//  paragraph must be included in any separate copy of this file.
"(in-package common-lisp-user)";

//  Definitions.
define class <atms> (<object>)
  slot atms-title = #f, init-keyword: #"atms-title";
  slot atms-node-counter = 0, init-keyword: #"atms-node-counter";
  //  unique namer for nodes.
  slot atms-just-counter = 0, init-keyword: #"atms-just-counter";
  //  unique namer for justifications.
  slot atms-env-counter = 0, init-keyword: #"atms-env-counter";
  //  Unique id for environments.
  slot atms-nodes = #f, init-keyword: #"atms-nodes";
  //  List of all atms nodes.
  slot atms-justs = #f, init-keyword: #"atms-justs";
  //  List of all justifications.
  slot atms-contradictions = #f, init-keyword: #"atms-contradictions";
  //  List of contradiction nodes.
  slot atms-assumptions = #f, init-keyword: #"atms-assumptions";
  //  List of all atms assumptions.
  slot atms-debugging = #f, init-keyword: #"atms-debugging";
  //  Trace grungy details.
  slot atms-nogood-table = #f, init-keyword: #"atms-nogood-table";
  slot atms-contra-node = #f, init-keyword: #"atms-contra-node";
  //  A dummy contradiction node.
  slot atms-env-table = #f, init-keyword: #"atms-env-table";
  slot atms-empty-env = #f, init-keyword: #"atms-empty-env";
  //  Empty environment.
  slot atms-node-string = #f, init-keyword: #"atms-node-string";
  slot atms-enqueue-procedure = #f, init-keyword: #"atms-enqueue-procedure";
end class <atms>;

define method print-atms (atms, stream, ignore)
  format(stream, "#<ATMS: %S>", atms.atms-title);
end method print-atms;

define class <tms-node> (<object>)
  slot tms-node-index = 0, init-keyword: #"tms-node-index";
  //  Unique name.
  slot tms-node-datum = #f, init-keyword: #"tms-node-datum";
  //  Pointer to IE data structures.
  slot tms-node-label = #f, init-keyword: #"tms-node-label";
  //  minimal envs believed under
  slot tms-node-justs = #f, init-keyword: #"tms-node-justs";
  //  providers of support
  slot tms-node-consequences = #f, init-keyword: #"tms-node-consequences";
  //  provides support for.
  slot tms-node-contradictory? = #f, init-keyword: #"tms-node-contradictory?";
  //  flag marking it as contradictory.
  slot tms-node-assumption? = #f, init-keyword: #"tms-node-assumption?";
  //  flag marking it as n assumption.
  slot tms-node-rules = #f, init-keyword: #"tms-node-rules";
  //  run when label non-empty.
  slot tms-node-atms = #f, init-keyword: #"tms-node-atms";
end class <tms-node>;

define method print-tms-node (node, stream, ignore)
  if (node.tms-node-assumption?)
    format(stream, "A-%D", node.tms-node-index);
  else
    format(stream, "#<NODE: %S>", node-string(node));
  end if;
end method print-tms-node;

define class <just> (<object>)
  slot just-index = 0, init-keyword: #"just-index";
  slot just-informant = #f, init-keyword: #"just-informant";
  slot just-consequence = #f, init-keyword: #"just-consequence";
  slot just-antecedents = #f, init-keyword: #"just-antecedents";
end class <just>;

define method print-just (just, stream, ignore)
  format(stream, "<%S %D>", just.just-informant, just.just-index);
end method print-just;

define class <env> (<object>)
  slot env-index = 0, init-keyword: #"env-index";
  slot env-count = 0, init-keyword: #"env-count";
  //  Number of assumptions.
  slot env-assumptions = #f, init-keyword: #"env-assumptions";
  slot env-nodes = #f, init-keyword: #"env-nodes";
  slot env-nogood? = #f, init-keyword: #"env-nogood?";
  slot env-rules = #f, init-keyword: #"env-rules";
end class <env>;

//  Call this if becomes nogood.
define method print-env-structure (env, stream, ignore)
  format(stream, "E-%D", env.env-index);
end method print-env-structure;

define method node-string (node)
  (node.tms-node-atms.atms-node-string)(node);
end method node-string;

// LTD: No macros.
#"debugging";

define method default-node-string (n)
  format(#f, "%S", n.tms-node-datum);
end method default-node-string;

define method ordered-insert (item, list, test)
  if (empty?(list))
    list(item);
  elseif (test(item, head(list)))
    pair(item, list);
  elseif (item == head(list))
    list;
  else
    pair(head(list), ordered-insert(item, tail(list), test));
  end if;
end method ordered-insert;

// LTD: No macros.
#"ordered-push";

define method assumption-order (a1, a2)
  a1.tms-node-index < a2.tms-node-index;
end method assumption-order;

define method env-order (e1, e2)
  e1.env-index < e2.env-index;
end method env-order;

//  Basic inference engine interface.
define method create-atms (title, #key node-string = #"default-node-string",
                           debugging = #f, enqueue-procedure = #f)
  let atms
      = make-atms(title: title, node-string: node-string,
                  debugging: debugging, enqueue-procedure: enqueue-procedure);
  atms.atms-contra-node
   := tms-create-node(atms, "The contradiction", contradictoryp: #t);
  atms.atms-empty-env := create-env(atms, #f);
  atms;
end method create-atms;

define method change-atms (atms, #key node-string, enqueue-procedure,
                           debugging)
  if (node-string) atms.atms-node-string := node-string; end if;
  if (debugging) atms.atms-debugging := debugging; end if;
  if (enqueue-procedure)
    atms.atms-enqueue-procedure := enqueue-procedure;
  end if;
end method change-atms;

define method true-node? (node)
  head(node.tms-node-label) == node.tms-node-atms.atms-empty-env;
end method true-node?;

define method in-node? (n, #key env)
  if (env)
    any?(method (le) subset-env?(le, env); end method, n.tms-node-label);
  else
    ~ empty?(n.tms-node-label);
  end if;
end method in-node?;

define method out-node? (n, env) ~ in-node?(n, env); end method out-node?;

define method node-consistent-with? (n, env)
  any?(method (le) ~ env-nogood?(union-env(le, env)); end method,
       n.tms-node-label);
end method node-consistent-with?;

define method tms-create-node (atms, datum, #key assumptionp, contradictoryp)
  node
   := make-tms-node(index: inc!(atms.atms-node-counter), datum: datum,
                    assumption?: assumptionp, contradictory?: contradictoryp,
                    atms: atms);
  push!(node, atms.atms-nodes);
  if (contradictoryp) push!(node, atms.atms-contradictions); end if;
  if (assumptionp)
    push!(node, atms.atms-assumptions);
    push!(create-env(atms, list(node)), node.tms-node-label);
  end if;
  node;
end method tms-create-node;

define method assume-node (node)
  if (~ node.tms-node-assumption?)
    atms := node.tms-node-atms;
    debugging(atms, "~%Converting ~A into an assumption", node);
    node.tms-node-assumption? := #t;
    push!(node, atms.atms-assumptions);
    update(list(create-env(atms, list(node))), node, #"assume-node");
  end if;
end method assume-node;

define method make-contradiction (node)
  if (~ node.tms-node-contradictory?)
    node.tms-node-contradictory? := #t;
    push!(node, atms.atms-contradictions);
    block (return)
      for (until %f)
        if (nogood := head(node.tms-node-label))
          new-nogood(atms, nogood, #"make-contradiction");
        else
          return(#f);
        end if;
      end for;
    end block;
  end if;
end method make-contradiction;

define method justify-node (informant, consequence, antecedents)
  begin
    atms := consequence.tms-node-atms;
    just
     := make-just(index: inc!(atms.atms-just-counter), informant: informant,
                  consequence: consequence, antecedents: antecedents);
  end;
  push!(just, consequence.tms-node-justs);
  for (node in antecedents) push!(just, node.tms-node-consequences); end for;
  push!(just, atms.atms-justs);
  debugging(atms, "~%Justifying ~A in terms of ~A on ~A", consequence,
            informant, map(node-string, antecedents));
  propagate(just, #f, list(atms.atms-empty-env));
  just;
end method justify-node;

define method nogood-nodes (informant, nodes)
  justify-node(informant, atms-contra-node(tms-node-atms(head(nodes))),
               nodes);
end method nogood-nodes;

//  Label updating
define method propagate (just, antecedent, envs)
  if (new-envs := weave(antecedent, envs, just.just-antecedents))
    update(new-envs, just.just-consequence, just);
  end if;
end method propagate;

define method update (new-envs, consequence, just)
  block (return-from-update)
    atms := consequence.tms-node-atms;
    if (consequence.tms-node-contradictory?)
      for (env in new-envs) new-nogood(atms, env, just); end for;
      return-from-update(#f);
    end if;
    new-envs := update-label(consequence, new-envs);
    if (~ new-envs) return-from-update(#f); end if;
    if (enqueuef := atms.atms-enqueue-procedure)
      for (rule in consequence.tms-node-rules) enqueuef(rule); end for;
      consequence.tms-node-rules := #f;
    end if;
    for (supported-just in consequence.tms-node-consequences)
      propagate(supported-just, consequence, new-envs);
      for (new-envs = new-envs then cdr(new-envs), until empty?(new-envs))
        if (~ member?(head(new-envs), consequence.tms-node-label))
          head(new-envs) := #f;
        end if;
      end for;
      new-envs := remove!(new-envs, #f);
      if (~ new-envs) return-from-update(#f); end if;
    end for;
  end block;
end method update;

define method update-label (node, new-envs)
  envs := node.tms-node-label;
  for (new-envs = new-envs then cdr(new-envs), until empty?(new-envs))
    for (nenvs = envs then cdr(nenvs), until empty?(nenvs))
      let _that = #f;
      if (_that := empty?(head(nenvs)))
        _that;
      elseif (_that := empty?(head(new-envs)))
        _that;
      elseif (_that
               := select (compare-env(car(new-envs), car(nenvs)))
                    (#"eq", #"s21")
                       => (head(new-envs) := #f);
                    #"s12"
                       => (env-nodes(head(nenvs))
                            := remove!(env-nodes(head(nenvs)),
                                       node,
                                       count: 1));
                           (head(nenvs) := #f);
                    otherwise
                       => #f;
                  end select)
        _that;
      end if;
    finally
      push!(head(new-envs), envs);
    end for;
  end for;
  new-envs := remove!(new-envs, #f);
  for (new-env in new-envs) push!(node, new-env.env-nodes); end for;
  node.tms-node-label := remove!(envs, #f);
  new-envs;
end method update-label;

define method weave (antecedent, envs, antecedents)
  block (return-from-weave)
    envs := copy-sequence(envs);
    for (node in antecedents)
      if (~ (node == antecedent))
        new-envs := #f;
        for (env in envs)
          if (env)
            for (node-env in node.tms-node-label)
              new-env := union-env(env, node-env);
              if (~ new-env.env-nogood?)
                block (return)
                  for (nnew-envs = new-envs then cdr(nnew-envs),
                       until empty?(nnew-envs))
                    if (head(nnew-envs))
                      select (compare-env(new-env, car(nnew-envs)))
                        (#"eq", #"s21")
                           => return(#f);
                        #"s12"
                           => head(nnew-envs) := #f;
                        otherwise
                           => #f;
                      end select;
                    end if;
                  finally
                    push!(new-env, new-envs);
                  end for;
                end block;
              end if;
            end for;
          end if;
        end for;
        envs := remove!(new-envs, #f);
        if (~ envs) return-from-weave(#f); end if;
      end if;
    end for;
    envs;
  end block;
end method weave;

define method in-antecedent? (nodes)
  empty?(nodes) | weave?(atms-empty-env(tms-node-atms(head(nodes))), nodes);
end method in-antecedent?;

define method weave? (env, nodes)
  if (empty?(nodes))
    #t;
  else
    block (return)
      for (e in tms-node-label(head(nodes)))
        new-env := union-env(e, env);
        if (~ new-env.env-nogood?)
          if (weave?(new-env, tail(nodes))) return(#t); end if;
        end if;
      end for;
    end block;
  end if;
end method weave?;

define method supporting-antecedent? (nodes, env)
  block (return)
    for (node in nodes)
      if (~ in-node?(node, env)) return(#f); end if;
    finally
      #t;
    end for;
  end block;
end method supporting-antecedent?;

define method remove-node (node)
  if (node.tms-node-consequences)
    error("Can't remove node with consequences");
  end if;
  atms := node.tms-node-atms;
  atms.atms-nodes := remove!(atms.atms-nodes, node, count: 1);
  for (just in node.tms-node-justs)
    for (ant in just.just-antecedents)
      ant.tms-node-consequences
       := remove!(ant.tms-node-consequences, just, count: 1);
    end for;
  end for;
  for (env in node.tms-node-label)
    env.env-nodes := remove!(env.env-nodes, node, count: 1);
  end for;
end method remove-node;

//  Creating and extending environments.
define method create-env (atms, assumptions)
  e
   := make-env(index: inc!(atms.atms-env-counter), assumptions: assumptions,
               count: size(assumptions));
  atms.atms-env-table := insert-in-table(atms.atms-env-table, e);
  set-env-contradictory(atms, e);
  e;
end method create-env;

define method union-env (e1, e2)
  if (e1.env-count > e2.env-count)
    let g108128 = e2;
    let g108129 = e1;
    e1 := g108128;
    e2 := g108129;
    #f;
  end if;
  block (return)
    for (assume in e1.env-assumptions)
      e2 := cons-env(assume, e2);
      if (e2.env-nogood?) return(#f); end if;
    end for;
  end block;
  e2;
end method union-env;

define method cons-env (assumption, env)
  nassumes
   := ordered-insert(assumption, env.env-assumptions, assumption-order);
  lookup-env(nassumes) | create-env(assumption.tms-node-atms, nassumes);
end method cons-env;

define method find-or-make-env (assumptions, atms)
  block (return-from-find-or-make-env)
    if (~ assumptions)
      return-from-find-or-make-env(atms.atms-empty-env);
    end if;
    //  Presumes the list of assumptions is ordered properly
    lookup-env(assumptions) | create-env(atms, assumptions);
  end block;
end method find-or-make-env;

//  Env tables.
define method insert-in-table (table, env)
  begin
    count := env.env-count;
    entry := cl-assoc(count, table, test: \=);
  end;
  if (entry)
    tail(entry) := pair(env, tail(entry));
    table;
  else
    ordered-insert(list(count, env), table,
                   method (entry1, entry2)
                     head(entry1) < head(entry2);
                   end method);
  end if;
end method insert-in-table;

define method lookup-env (assumes)
  block (return)
    for (env in tail(cl-assoc(size(assumes),
                              atms-env-table(tms-node-atms(head(assumes))),
                              test: \=)))
      if (env.env-assumptions = assumes) return(env); end if;
    end for;
  end block;
end method lookup-env;

define method subset-env? (e1, e2)
  if (e1 == e2)
    #t;
  elseif (e1.env-count > e2.env-count)
    #f;
  else
    subset?(e1.env-assumptions, e2.env-assumptions);
  end if;
end method subset-env?;

define method compare-env (e1, e2)
  if (e1 == e2)
    #"eq";
  elseif (e1.env-count < e2.env-count)
    if (subset?(e1.env-assumptions, e2.env-assumptions)) #"s12"; end if;
  elseif (subset?(e2.env-assumptions, e1.env-assumptions))
    #"s21";
  end if;
end method compare-env;

//  Processing nogoods
define method new-nogood (atms, cenv, just)
  debugging(atms, format(#f, "\n  %S new minimal nogood.", cenv));
  cenv.env-nogood? := just;
  remove-env-from-labels(cenv, atms);
  atms.atms-nogood-table := insert-in-table(atms.atms-nogood-table, cenv);
  count := cenv.env-count;
  for (entry in atms.atms-nogood-table)
    if (head(entry) > count)
      for (old in tail(entry))
        if (subset-env?(cenv, old))
          tail(entry) := remove!(tail(entry), old, count: 1);
        end if;
      end for;
    end if;
  end for;
  for (entry in atms.atms-env-table)
    if (head(entry) > count)
      for (old in tail(entry))
        if (~ old.env-nogood? & subset-env?(cenv, old))
          old.env-nogood? := cenv;
          remove-env-from-labels(old, atms);
        end if;
      end for;
    end if;
  end for;
end method new-nogood;

define method set-env-contradictory (atms, env)
  if (env.env-nogood?)
    #t;
  else
    count := env.env-count;
    block (return)
      for (entry in atms.atms-nogood-table)
        if (head(entry) > count)
          return(#f);
        else
          block (return)
            for (cenv in tail(entry))
              if (subset-env?(cenv, env))
                env.env-nogood? := cenv;
                return(#t);
              end if;
            end for;
          end block;
        end if;
      end for;
    end block;
  end if;
end method set-env-contradictory;

define method remove-env-from-labels (env, atms)
  if (enqueuef := atms.atms-enqueue-procedure)
    for (rule in env.env-rules) enqueuef(rule); end for;
    env.env-rules := #f;
  end if;
  for (node in env.env-nodes)
    node.tms-node-label := remove!(node.tms-node-label, env, count: 1);
  end for;
end method remove-env-from-labels;

//  Interpretation construction
#f;

define method interpretations (atms, choice-sets, #key defaults)
  block (return-from-interpretations)
    if (atms.atms-debugging)
      format(*trace-output*,
             "\n Constructing interpretations depth-first...");
    end if;
    fluid-bind (*solutions* = #f)
      let choice-sets
          = map(method (alt-set)
                  apply(concatenate!,
                        map(method (alt)
                              copy-sequence(alt.tms-node-label);
                            end method,
                            alt-set));
                end method,
                choice-sets);
      for (choice in head(choice-sets))
        get-depth-solutions1(choice, tail(choice-sets));
      end for;
      *solutions* := remove!(*solutions*, #f);
      if (~ *solutions*)
        if (choice-sets)
          return-from-interpretations(#f);
        else
          *solutions* := list(atms.atms-empty-env);
        end if;
      end if;
      if (defaults)
        begin solutions := *solutions*; *solutions* := #f; end;
        for (solution in solutions)
          extend-via-defaults(solution, defaults, defaults);
        end for;
      end if;
      remove!(*solutions*, #f);
    end fluid-bind;
  end block;
end method interpretations;

define method get-depth-solutions1 (solution, choice-sets)
  let _that = #f;
  if (empty?(choice-sets))
    if (~ block (return)
            for (old-solutions = *solutions* then cdr(old-solutions),
                 until empty?(old-solutions))
              if (head(old-solutions))
                select (compare-env(car(old-solutions), solution))
                  (#"eq", #"s12")
                     => return(#t);
                  #"s21"
                     => (head(old-solutions) := #f);
                  otherwise
                     => #f;
                end select;
              end if;
            end for;
          end block)
      push!(solution, *solutions*);
    end if;
  elseif (_that := solution.env-nogood?)
    _that;
    // something died.
    else
    for (choice in head(choice-sets))
      new-solution := union-env(solution, choice);
      if (~ new-solution.env-nogood?)
        get-depth-solutions1(new-solution, tail(choice-sets));
      end if;
    end for;
  end if;
end method get-depth-solutions1;

define method extend-via-defaults (solution, remaining, original)
  for (new-solution = nil then nil, defaults = remaining then cdr(defaults),
       until empty?(defaults))
    new-solution := cons-env(head(defaults), solution);
    if (~ new-solution.env-nogood?)
      extend-via-defaults(new-solution, tail(defaults), original);
    end if;
  finally
    member?(solution, *solutions*)
     | block (return)
         for (default in original)
           (member?(default, solution.env-assumptions)
             | env-nogood?(cons-env(default, solution))
             | return(#t));
         end for;
       end block
     | push!(solution, *solutions*);
  end for;
end method extend-via-defaults;

//  Generating explanations
//  This returns a list of justifications which form a DAG for the 
//  derivation. This is quite complicated because this is really a 
//  simple consequent JTMS.
define method explain-node (node, env)
  explain-node-1(env, node, #f, #f);
end method explain-node;

define method explain-node-1 (env, node, queued-nodes, explanation)
  block (return-from-explain-node-1)
    let _that = #f;
    if (member?(node, queued-nodes))
      #f;
    elseif (node.tms-node-assumption? & member?(node, env.env-assumptions))
      pair(pair(#"assume", node), explanation);
    elseif (_that
             := block (return)
                  for (just in explanation)
                    if (if (instance?(just, <list>))
                          tail(just) == node;
                        else
                          just.just-consequence == node;
                        end if)
                      return(explanation);
                    end if;
                  end for;
                end block)
      _that;
    else
      queued-nodes := pair(node, queued-nodes);
      for (just in node.tms-node-justs)
        if (~ block (return)
                for (a in just.just-antecedents)
                  if ((~ in-node?(a, env))) return(#t); end if;
                end for;
              end block)
          let new-explanation = explanation;
          block (return)
            for (a in just.just-antecedents)
              new-explanation
               := explain-node-1(env, a, queued-nodes, new-explanation);
              if (~ new-explanation) return(#f); end if;
            finally
              return-from-explain-node-1(pair(just, new-explanation));
            end for;
          end block;
        end if;
      end for;
    end if;
  end block;
end method explain-node-1;

//  Printing
define method why-node (node, #key stream = #t, prefix = "")
  format(stream, "\n<%S%S,{", prefix, node.tms-node-datum);
  for (e in node.tms-node-label) env-string(e, stream); end for;
  format(stream, "}>");
end method why-node;

define method why-nodes (atms, #key stream = #t)
  for (n in reverse(atms.atms-nodes)) why-node(n, stream); end for;
end method why-nodes;

define method node-justifications (node, #key stream = #t)
  format-out("\n For %S:", node-string(node));
  for (j in node.tms-node-justs) print-justification(j, stream); end for;
end method node-justifications;

define method print-justification (j, #key stream = #t)
  format(stream, "\n  %S, ", j.just-informant);
  for (a in j.just-antecedents) why-node(a, stream, "     "); end for;
end method print-justification;

define method e (atms, n)
  block (return-from-e)
    for (bucket in atms.atms-env-table)
      for (env in tail(bucket))
        if (env.env-index = n) return-from-e(env); end if;
      end for;
    end for;
  end block;
end method e;

define method print-env (e, #key stream = #t)
  format(stream, "\n%S:%S", e, if (e.env-nogood?) "* "; else " "; end if);
  env-string(e, stream);
end method print-env;

define method env-string (e, #key stream)
  assumptions := e.env-assumptions;
  if (assumptions)
    printer := atms-node-string(tms-node-atms(head(assumptions)));
  end if;
  for (a in assumptions) push!(printer(a), strings); end for;
  (method (s, #rest args)
     block (return)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               block (return)
                 block (return)
                   write-char++('{', xp);
                   let args = pop!(args);
                   block (return)
                     block (return)
                       block (return)
                         local method go-l ()
                                 if (empty?(args)) return(#f); end if;
                                 fluid-bind (*print-escape* = #f)
                                   write+(pop!(args), xp);
                                 end fluid-bind;
                                 if (empty?(args))
                                   return-from-nil(#f);
                                 end if;
                                 write-char++(',', xp);
                                 go-l();
                               end method go-l;
                         go-l();
                       end block;
                     end block;
                   end block;
                   write-char++('}', xp);
                 end block;
                 if (args) copy-sequence(args); end if;
               end block;
             end method,
             s, args);
     end block;
   end method)(stream, sort!(strings, test: string-less?));
end method env-string;

//  Printing global data
define method print-nogoods (atms, #key stream = #t)
  print-env-table(atms.atms-nogood-table, stream);
end method print-nogoods;

define method print-envs (atms, #key stream = #t)
  print-env-table(atms.atms-env-table, stream);
end method print-envs;

define method print-env-table (table, stream)
  for (bucket in table)
    for (env in tail(bucket)) print-env(env, stream); end for;
  end for;
end method print-env-table;

define method print-atms-statistics (atms)
  print-table("~% For env table:", atms.atms-env-table);
  print-table("~% For nogood table:", atms.atms-nogood-table);
end method print-atms-statistics;

define method print-table (msg, table)
  msg(#t);
  for (entry in table)
    format-out("\n   Length %D, %D", head(entry), size(tail(entry)));
  end for;
end method print-table;

