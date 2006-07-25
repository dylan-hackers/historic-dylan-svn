// -*- Mode:  LISP; Syntax: Common-lisp; Package: USER -*-
//  Justification-based Truth Maintenence System (JTMS)
//  Version 176.
//  Last edited 1/29/93, by KDF
//  Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
//  and Johan de Kleer, the Xerox Corporation.
//  All rights reserved.
//  See the file legal.txt for a paragraph stating scope of permission
//  and disclaimer of warranty.  The above copyright notice and that
//  paragraph must be included in any separate copy of this file.
"(in-package common-lisp-user)";

define class <jtms> (<object>)
  slot jtms-title = #f, init-keyword: #"jtms-title";
  slot jtms-node-counter = 0, init-keyword: #"jtms-node-counter";
  //  unique namer for nodes.
  slot jtms-just-counter = 0, init-keyword: #"jtms-just-counter";
  //  unique namer for justifications.
  slot jtms-nodes = #f, init-keyword: #"jtms-nodes";
  //  list of all tms nodes.
  slot jtms-justs = #f, init-keyword: #"jtms-justs";
  //  list of all justifications
  slot jtms-debugging = #f, init-keyword: #"jtms-debugging";
  //  debugging flag
  slot jtms-contradictions = #f, init-keyword: #"jtms-contradictions";
  //  list of contradiction nodes.
  slot jtms-assumptions = #f, init-keyword: #"jtms-assumptions";
  //  list of assumption nodes.
  slot jtms-checking-contradictions = #t,
       init-keyword: #"jtms-checking-contradictions";
  //  For external systems
  slot jtms-node-string = #f, init-keyword: #"jtms-node-string";
  slot jtms-contradiction-handler = #f,
       init-keyword: #"jtms-contradiction-handler";
  slot jtms-enqueue-procedure = #f, init-keyword: #"jtms-enqueue-procedure";
end class <jtms>;

define method print-jtms (jtms, stream, ignore)
  format(stream, "#<JTMS: %S>", jtms.jtms-title);
end method print-jtms;

define class <tms-node> (<object>)
  slot tms-node-index = 0, init-keyword: #"tms-node-index";
  slot tms-node-datum = #f, init-keyword: #"tms-node-datum";
  //  pointer to external problem solver
  slot tms-node-label = #"out", init-keyword: #"tms-node-label";
  //  :IN means believed, :OUT means disbelieved
  slot tms-node-support = #f, init-keyword: #"tms-node-support";
  //  Current justification or premise marker
  slot tms-node-justs = #f, init-keyword: #"tms-node-justs";
  //  Possible justifications
  slot tms-node-consequences = #f, init-keyword: #"tms-node-consequences";
  //  Justifications in which it is an antecedent
  slot tms-node-mark = #f, init-keyword: #"tms-node-mark";
  //  Marker for sweep algorithms
  slot tms-node-contradictory? = #f, init-keyword: #"tms-node-contradictory?";
  //  Flag marking it as contradictory
  slot tms-node-assumption? = #f, init-keyword: #"tms-node-assumption?";
  //  Flag marking it as an assumption.
  slot tms-node-in-rules = #f, init-keyword: #"tms-node-in-rules";
  //  Rules that should be triggered when node goes in
  slot tms-node-out-rules = #f, init-keyword: #"tms-node-out-rules";
  //  Rules that should be triggered when node goes out
  slot tms-node-jtms = #f, init-keyword: #"tms-node-jtms";
end class <tms-node>;

//  The JTMS in which this node appears.
define method print-tms-node (node, stream, ignore)
  format(stream, "#<Node: %S>", node-string(node));
end method print-tms-node;

define class <just> (<object>)
  slot just-index = 0, init-keyword: #"just-index";
  slot just-informant, init-keyword: #"just-informant";
  slot just-consequence, init-keyword: #"just-consequence";
  slot just-antecedents, init-keyword: #"just-antecedents";
end class <just>;

define method print-just (just, stream, ignore)
  format(stream, "#<Just %D>", just.just-index);
end method print-just;

define method tms-node-premise? (node)
  (support := node.tms-node-support) & ~ (support == #"enabled-assumption")
   & empty?(support.just-antecedents);
end method tms-node-premise?;

//  Simple utilities:
define method node-string (node)
  (node.tms-node-jtms.jtms-node-string)(node);
end method node-string;

// LTD: No macros.
#"debugging-jtms";

define method tms-error (string, node)
  error(string, node-string(node));
end method tms-error;

define method default-node-string (n)
  format(#f, "%S", n.tms-node-datum);
end method default-node-string;

define method create-jtms (title, #key node-string = #"default-node-string",
                           debugging, checking-contradictions = #t,
                           contradiction-handler = #"ask-user-handler",
                           enqueue-procedure)
  make-jtms(title: title, node-string: node-string, debugging: debugging,
            checking-contradictions: checking-contradictions,
            contradiction-handler: contradiction-handler,
            enqueue-procedure: enqueue-procedure);
end method create-jtms;

define method change-jtms (jtms, #key contradiction-handler, node-string,
                           enqueue-procedure, debugging,
                           checking-contradictions)
  if (node-string) jtms.jtms-node-string := node-string; end if;
  if (debugging) jtms.jtms-debugging := debugging; end if;
  if (checking-contradictions)
    jtms.jtms-checking-contradictions := checking-contradictions;
  end if;
  if (contradiction-handler)
    jtms.jtms-contradiction-handler := contradiction-handler;
  end if;
  if (enqueue-procedure)
    jtms.jtms-enqueue-procedure := enqueue-procedure;
  end if;
end method change-jtms;

//  Basic inference-engine interface.
define method in-node? (node) node.tms-node-label == #"in"; end method in-node?;

define method out-node? (node)
  node.tms-node-label == #"out";
end method out-node?;

define method tms-create-node (jtms, datum, #key assumptionp, contradictoryp)
  let node
      = make-tms-node(index: inc!(jtms.jtms-node-counter), datum: datum,
                      assumption?: assumptionp,
                      contradictory?: contradictoryp, jtms: jtms);
  if (assumptionp) push!(node, jtms.jtms-assumptions); end if;
  if (contradictoryp) push!(node, jtms.jtms-contradictions); end if;
  push!(node, jtms.jtms-nodes);
  node;
end method tms-create-node;

//  Converts a regular node to an assumption and enables it.
define method assume-node (node)
  if (~ node.tms-node-assumption?)
    debugging-jtms(jtms, "~%Converting ~A into an assumption", node);
    node.tms-node-assumption? := #t;
  end if;
  enable-assumption(node);
end method assume-node;

define method make-contradiction (node)
  if (~ node.tms-node-contradictory?)
    node.tms-node-contradictory? := #t;
    push!(node, jtms.jtms-contradictions);
    check-for-contradictions(jtms);
  end if;
end method make-contradiction;

define method justify-node (informant, consequence, antecedents)
  begin
    jtms := consequence.tms-node-jtms;
    just
     := make-just(index: inc!(jtms.jtms-just-counter), informant: informant,
                  consequence: consequence, antecedents: antecedents);
  end;
  push!(just, consequence.tms-node-justs);
  for (node in antecedents) push!(just, node.tms-node-consequences); end for;
  push!(just, jtms.jtms-justs);
  debugging-jtms(jtms, "~%Justifying ~A by ~A using ~A.", consequence,
                 informant, map(node-string, antecedents));
  if (antecedents | out-node?(consequence))
    if (check-justification(just)) install-support(consequence, just); end if;
  else
    consequence.tms-node-support := just;
  end if;
  check-for-contradictions(jtms);
end method justify-node;

//  Support for adding justifications
define method check-justification (just)
  out-node?(just.just-consequence) & justification-satisfied?(just);
end method check-justification;

define method justification-satisfied? (just)
  every?(in-node?, just.just-antecedents);
end method justification-satisfied?;

define method install-support (conseq, just)
  make-node-in(conseq, just);
  propagate-inness(conseq);
end method install-support;

define method propagate-inness (node)
  for (until empty?(node := pop!(q)))
    debugging-jtms(jtms, "~%   Propagating belief in ~A.", node);
    for (justification in node.tms-node-consequences)
      if (check-justification(justification))
        make-node-in(justification.just-consequence, justification);
        push!(justification.just-consequence, q);
      end if;
    end for;
  end for;
end method propagate-inness;

define method make-node-in (conseq, reason)
  begin
    jtms := conseq.tms-node-jtms;
    enqueuef := jtms.jtms-enqueue-procedure;
  end;
  debugging-jtms(jtms, "~%     Making ~A in via ~A.", conseq,
                 if (instance?(reason, <symbol>))
                   reason;
                 else
                   pair(reason.just-informant,
                        map(jtms.jtms-node-string, reason.just-antecedents));
                 end if);
  conseq.tms-node-label := #"in";
  conseq.tms-node-support := reason;
  if (enqueuef)
    for (in-rule in conseq.tms-node-in-rules) enqueuef(in-rule); end for;
    conseq.tms-node-in-rules := #f;
  end if;
end method make-node-in;

//  Assumption Manipulation
define method retract-assumption (node)
  if (node.tms-node-support == #"enabled-assumption")
    jtms := node.tms-node-jtms;
    debugging-jtms(jtms, "~%  Retracting assumption ~A.", node);
    make-node-out(node);
    find-alternative-support(jtms, pair(node, propagate-outness(node, jtms)));
  end if;
end method retract-assumption;

define method enable-assumption (node)
  if (~ node.tms-node-assumption?)
    tms-error("Can't enable the non-assumption ~A", node);
  end if;
  debugging-jtms(jtms, "~%  Enabling assumption ~A.", node);
  let _that = #f;
  if (out-node?(node))
    make-node-in(node, #"enabled-assumption");
    propagate-inness(node);
  elseif (_that
           := node.tms-node-support == #"enabled-assumption"
               | empty?(node.tms-node-support.just-antecedents))
    _that;
  else
    node.tms-node-support := #"enabled-assumption";
  end if;
  check-for-contradictions(jtms);
end method enable-assumption;

define method make-node-out (node)
  begin
    jtms := node.tms-node-jtms;
    enqueuef := jtms.jtms-enqueue-procedure;
  end;
  debugging-jtms(jtms, "~%     retracting belief in ~a.", node);
  node.tms-node-support := #f;
  node.tms-node-label := #"out";
  if (enqueuef)
    for (out-rule in node.tms-node-out-rules) enqueuef(out-rule); end for;
  end if;
  node.tms-node-out-rules := #f;
end method make-node-out;

define method propagate-outness (node, jtms)
  debugging-jtms(jtms, "~%   Propagating disbelief in ~A.", node);
  for (js = node.tms-node-consequences then append(cdr(js), new),
       new = nil then nil, conseq = nil then nil, until empty?(js))
    // For each justification using the node, check to see if
    // it supports some other node.  If so, forget that node,
    // queue up the node to look for other support, and recurse
    conseq := just-consequence(head(js));
    if (conseq.tms-node-support == head(js))
      make-node-out(conseq);
      push!(conseq, out-queue);
      new := conseq.tms-node-consequences;
    end if;
  finally
    out-queue;
  end for;
end method propagate-outness;

define method find-alternative-support (jtms, out-queue)
  debugging-jtms(jtms, "~%   Looking for alternative supports.");
  for (node in out-queue)
    if (~ in-node?(node))
      block (return)
        for (just in node.tms-node-justs)
          if (check-justification(just))
            install-support(just.just-consequence, just);
            return(just);
          end if;
        end for;
      end block;
    end if;
  end for;
end method find-alternative-support;

//  Contradiction handling interface
define method check-for-contradictions (jtms)
  if (jtms.jtms-checking-contradictions)
    for (cnode in jtms.jtms-contradictions)
      if (in-node?(cnode)) push!(cnode, contradictions); end if;
    end for;
    if (contradictions)
      (jtms.jtms-contradiction-handler)(jtms, contradictions);
    end if;
  end if;
end method check-for-contradictions;

// LTD: No macros.
#"without-contradiction-check";

// LTD: No macros.
#"with-contradiction-check";

define method contradiction-check (jtms, flag, body)
  let jtmsv = generate-symbol();
  let old-value = generate-symbol();
  bq-list(#"let*",
          bq-list(bq-list(jtmsv, jtms),
                  bq-list(old-value,
                          bq-list(#"jtms-checking-contradictions", jtmsv))),
          bq-list(#"unwind-protect",
                  bq-list*(#"progn",
                           bq-list(#"setf",
                                   bq-list(#"jtms-checking-contradictions",
                                           jtmsv),
                                   flag),
                           body),
                  bq-list(#"setf",
                          bq-list(#"jtms-checking-contradictions", jtmsv),
                          old-value)));
end method contradiction-check;

// LTD: No macros.
#"with-contradiction-handler";

define method default-assumptions (jtms)
  with-contradiction-check(jtms,
                           with-contradiction-handler(jtms,
                                                      method (#rest ignore)
                                                      contradiction(#t);
                                                      end method,
                                                      for (assumption in jtms
                                                                         .jtms-assumptions)
                                                      let _that = #f;
                                                      if (_that
                                                           := assumption
                                                              .tms-node-support
                                                               == #"enabled-assumption")
                                                      _that;
                                                      elseif (_that
                                                               := ~ (#"default"
                                                                      == assumption
                                                                         .tms-node-assumption?))
                                                      _that;
                                                      elseif (block (contradiction)
                                                              enable-assumption(assumption);
                                                              end block)
                                                      retract-assumption(assumption);
                                                      end if;
                                                      end for));
end method default-assumptions;

//  Well-founded support inqueries
define method supporting-justification-for-node (node)
  node.tms-node-support;
end method supporting-justification-for-node;

define method assumptions-of-node (node)
  for (nodes = list(node) then append(cdr(nodes), new), new = nil then nil,
       until empty?(nodes))
    let node = head(nodes);
    let _that = #f;
    if (_that := node.tms-node-mark == marker)
      _that;
    elseif (node.tms-node-support == #"enabled-assumption")
      push!(node, assumptions);
    elseif (in-node?(node))
      new := node.tms-node-support.just-antecedents;
    end if;
    node.tms-node-mark := marker;
  finally
    assumptions;
  end for;
end method assumptions-of-node;

define method enabled-assumptions (jtms)
  for (assumption in jtms.jtms-assumptions)
    if (assumption.tms-node-support == #"enabled-assumption")
      push!(assumption, result);
    end if;
  finally
    result;
  end for;
end method enabled-assumptions;

//  Inference engine stub to allow this JTMS to be used stand alone
define method why-node (node)
  justification := node.tms-node-support;
  if (justification == #"enabled-assumption")
    format-out("\n%S is an enabled assumption", node-string(node));
  elseif (justification)
    format-out("\n%S is IN via %S on", node-string(node),
               justification.just-informant);
    for (anode in justification.just-antecedents)
      format-out("\n  %S", node-string(anode));
    end for;
  else
    format-out("\n%S is OUT.", node-string(node));
  end if;
  node;
end method why-node;

define method why-nodes (jtms)
  for (node in jtms.jtms-nodes) why-node(node); end for;
end method why-nodes;

#f;

define method ask-user-handler (jtms, contradictions)
  handle-one-contradiction(head(contradictions));
  check-for-contradictions(jtms);
end method ask-user-handler;

define method handle-one-contradiction (contra-node)
  *contra-assumptions* := assumptions-of-node(contra-node);
  if (~ *contra-assumptions*)
    tms-error("~%There is a flaw in the universe...~A", contra-node);
  end if;
  format-out("\nContradiction found: %S", node-string(contra-node));
  print-contra-list(*contra-assumptions*);
  format-out("\nCall (TMS-ANSWER <number>) to retract assumption.");
  the-answer
   := block (tms-contradiction-handler)
        break("JTMS contradiction break");
      end block;
  if (instance?(the-answer, <integer>) & the-answer > 0
       & ~ (the-answer > size(*contra-assumptions*)))
    retract-assumption(*contra-assumptions*[the-answer - 1]);
  end if;
end method handle-one-contradiction;

define method print-contra-list (nodes)
  for (counter = 1 then 1+(counter), nn = nodes then cdr(nn),
       until empty?(nn))
    format-out("\n%S %S", counter, node-string(head(nn)));
  end for;
end method print-contra-list;

define method tms-answer (num)
  if (instance?(num, <integer>))
    if (num > 0)
      if (~ (num > size(*contra-assumptions*)))
        tms-contradiction-handler(num);
      else
        format-out("\nIgnoring answer, too big.");
      end if;
    else
      format-out("\nIgnoring answer, too small");
    end if;
  else
    format-out("\nIgnoring answer, must be an integer.");
  end if;
end method tms-answer;

define method explore-network (node)
  block (return-from-explore-network)
    if (~ in-node?(node))
      format-out("\n Sorry, %S not believed.", node-string(node));
      return-from-explore-network(node);
    end if;
    for (stack = nil then nil, current = node then node,
         options = nil then nil, olen = 0 then 0, done? = nil then nil,
         until done?)
      why-node(current);
      options
       := if (instance?(current.tms-node-support, <just>))
            current.tms-node-support.just-antecedents;
          end if;
      olen := size(options);
      for (good? = nil then nil, choice = 0 then 0, until good?)
        format-out("\n>>>");
        choice
         := // LTD: Function READ not yet implemented.
            read();
        if (choice == #"q"
             | (instance?(choice, <integer>) & ~ (choice > olen)
                 & ~ (choice < 0)))
          good? := choice;
        else
          format-out("\n Must be q or an integer from 0 to %D.", olen);
        end if;
      finally
        select (good?)
          #"q"
             => return-from-explore-network(current);
          0
             => if (stack)
                  current := pop!(stack);
                else
                  return-from-explore-network(current);
                end if;
          otherwise
             => push!(current, stack); current := options[good? - 1];
        end select;
      end for;
    finally
      current;
    end for;
  end block;
end method explore-network;

