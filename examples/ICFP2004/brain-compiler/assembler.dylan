module: assembler


define macro brain-definer
  { define brain ?:name ?states end }
    => { define function ?name() => brain :: <vector>;
           let instrs = make(<table>);
           let (label, counter) = values(start:, 0);
           ?states;
           compile-states(instrs)
         end function }

states:
  { } => { }
  { [?label:expression] ?state; ... }
    => { let (label, counter) = values(?label, 0); ?state; ... }
  { ?state; ... }
    => { let counter = counter + 1; ?state; ... }

 state:
  { Drop, (?label:name) }
    => { push-thunk(instrs, label, counter,
                    method()
                        make(<drop>, state: lookup(instrs, ?#"label", 0))
                    end) }

  { Drop }
    => { push-thunk(instrs, label, counter,
                    method()
                        make(<drop>, state: lookup(instrs, label, counter + 1))
                    end) }

  { Turn ?:name, (?label:name) }
    => { push-thunk(instrs, label, counter,
                    method()
                        make(<turn>,
                             left-or-right: ?#"name",
                             state: lookup(instrs, ?#"label", 0))
                    end) }

  { Turn ?:name }
    => { push-thunk(instrs, label, counter,
                    method()
                        make(<turn>,
                             left-or-right: ?#"name",
                             state: lookup(instrs, label, counter + 1))
                    end) }

  { Move ?success:name => ?fail:name }
    => { push-thunk(instrs, label, counter,
                    method() make(<move>,
                                  state-success:
                                    lookup(instrs, ?#"success", 0),
                                  state-failure:
                                    lookup(instrs, ?#"fail", 0))
                    end) }

  { Move => ?:name }
    => { push-thunk(instrs, label, counter,
                    method() make(<move>,
                                  state-success:
                                    lookup(instrs, label, counter + 1),
                                  state-failure:
                                    lookup(instrs, ?#"name", 0))
                    end) }

  { Flip ?prob:expression, (?yes:name, ?no:name) }
    => { push-thunk(instrs, label, counter,
                    method()
                        make(<flip>,
                             probability: ?prob,
                             state-success: lookup(instrs, ?#"yes", 0),
                             state-failure: lookup(instrs, ?#"no", 0))
                    end method) }

  { Sense ?where:name ?what:name, (?yes:name, ?no:name) }
    => { push-thunk(instrs, label, counter,
                    method() make(<sense>,
                                  direction: ?#"where",
                                  condition: ?#"what",
                                  state-true: lookup(instrs, ?#"yes", 0),
                                  state-false: lookup(instrs, ?#"no", 0))
                    end) }

  { Sense ?what:name, (?yes:name, ?no:name) }
    => { push-thunk(instrs, label, counter,
                    method() make(<sense>,
                                  direction: #"Here",
                                  condition: ?#"what",
                                  state-true: lookup(instrs, ?#"yes", 0),
                                  state-false: lookup(instrs, ?#"no", 0))
                    end) }
end macro;


define function push-thunk (instrs, label, counter, thunk) => ();
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
    <function> =>
      instrs[pos] := instr();
    otherwise =>
      instr;
  end;
end;

define function compile-states (instrs :: <table>)
 => brain :: <vector>;
  let brain :: <stretchy-vector> = make(<vector>);
  let start-instr = instrs[start:];
  let pos-table :: <table> = make(<table>);
  put-instruction(start-instr, brain, pos-table);
  
end;

define generic put-instruction(instr :: <instruction>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();

define method put-instruction(instr :: <instruction>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  pos-table[instr] := brain.size;
  add!(brain, instr);
end;

define method put-instruction(instr :: <drop>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  
  let next-state = instr.state;
  select (next-state by instance?)
//    <function> =>
//      instr.state := next-state();
//      put-instruction(instr, brain, pos-table);
    <instruction> =>
      let pos = element(pos-table, next-state, default: #f);
      if (pos)
        instr.state := pos;
      else
        put-instruction(next-state, brain, pos-table);
        put-instruction(instr, brain, pos-table);
      end;
    <integer> =>
      #t // we are done
  end select;
end;


define function dump-brain(brain :: <vector>)
  map(compose(format-out, unparse), brain)
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
