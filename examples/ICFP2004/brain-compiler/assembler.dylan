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
end;



define functional class <instruction-label-count>
  slot instruction-label, required-init-keyword: label:;
  slot instruction-count, required-init-keyword: count:;
end;

define method functional-== (l :: <instruction-label-count>, r :: <instruction-label-count>)
 => same :: <boolean>;
  l.instruction-label == r.instruction-label
  & l.instruction-count == r.instruction-count
end;

