module: assembler


define macro brain-definer
  { define brain ?:name ?states end }
   =>
  { define function ?name() let instrs = make(<table>); let (label, counter) = (start:, 0); ?states end }

 states:
  {} => {}
  { ?state; ... } => { let counter = counter + 1; ?state; ... }
  { ?label:expression ?state; ... } => { let (label, counter) = (?label, 0); ?state; ... }

 state:
  { Turn ?:name } => { instrs[make(<instruction-label-count>, label: label, count: counter)]
                         := method() make(<turn>, left-or-right: ?#"name", state: lookup(instrs, label, counter + 1)) end; }
  { Turn ?:name (?label:name) } => { instrs[make(<instruction-label-count>, label: label, count: counter)]
                                       := method() make(<turn>, left-or-right: ?#"name", state: lookup(instrs, ?#"label", 0)) end; }


  { Move => ?:name } => { push-thunk(instrs, label, counter,
                                     method() make(<move>,
                                                   state-success: lookup(instrs, label, counter + 1),
                                                   state-failure: lookup(instrs, ?#"name", 0))
                                     end)
                        }

  { Move ?success:name => ?fail:name } => { push-thunk(instrs, label, counter,
                                                       method() make(<move>,
                                                                      state-success: lookup(instrs, ?#"success", 0),
                                                                      state-failure: lookup(instrs, ?#"fail", 0))
                                                       end)
                        }

  { Flip ?prob:expression (?yes:name, ?no:name) } => { push-thunk(instrs, label, counter,
                                                                  method() make(<flip>,
                                                                                probability: ?prob,
                                                                                state-success: lookup(instrs, ?#"yes", 0),
                                                                                state-failure: lookup(instrs, ?#"no", 0))
                                                                   end)
                        }
end;


define function push-thunk (instrs, label, counter, thunk)
  let pos = make(<instruction-label-count>, label: label, count: counter);
  if (element(instrs, pos, default: #f))
    error("label %s already defined?", label);
  end;
  
  instrs[pos] := thunk;
end;

define function lookup (instrs, label, counter)
 => instr :: <instruction>;
  let pos = make(<instruction-label-count>, label: label, count: counter);
  let instr = instrs[pos];
  select (instr by instance?)
    <function> => instrs[pos] := instr();
    otherwise: instr;
  end;
end;


define functional class <instruction-label-count> (<object>)
  constant slot instruction-label, required-init-keyword: label:;
  constant slot instruction-count, required-init-keyword: count:;
end;

define method functional-==
    (l :: <instruction-label-count>, r :: <instruction-label-count>)
 => (same :: <boolean>);
  l.instruction-label == r.instruction-label
    & l.instruction-count == r.instruction-count
end;
