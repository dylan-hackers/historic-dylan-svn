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
  { Verbatim { ?:expression } }
    => { push-thunk(instrs, label, counter,
                    method()
                        ?expression
                    end) }

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


  { PickUp ?success:name => ?fail:name }
    => { push-thunk(instrs, label, counter,
                    method() make(<pickup>,
                                  state-success:
                                    lookup(instrs, ?#"success", 0),
                                  state-failure:
                                    lookup(instrs, ?#"fail", 0))
                    end) }

  { PickUp => ?:name }
    => { push-thunk(instrs, label, counter,
                    method() make(<pickup>,
                                  state-success:
                                    lookup(instrs, label, counter + 1),
                                  state-failure:
                                    lookup(instrs, ?#"name", 0))
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
 format-out("push-thunk: (%s, %d)\n", label, counter);
// flush-stream(*standard-output*);
  let pos = make(<instruction-label-count>, label: label, count: counter);
  if (element(instrs, pos, default: #f))
    error("label %s already defined?", label);
  end;
  
  instrs[pos] := thunk;
end;

define function lookup (instrs, label, counter)
 => instr :: <instruction>;
 
 
 
 format-out("lookup: (%s, %d)\n", label, counter);
 
 
  let pos = make(<instruction-label-count>, label: label, count: counter);
  let instr = instrs[pos];

 format-out("found: (%s, %d)\n", label, counter);
  select (instr by instance?)
    <function> =>
      instrs[pos] := instr();
    otherwise =>
      instr;
  end;
end;

define function compile-states (instrs :: <table>)
 => brain :: <vector>;
  let brain :: <stretchy-vector> = make(<stretchy-vector>);
  let start-instr = instrs[make(<instruction-label-count>, label: start:, count: 0)];
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

define macro integrate-state
  { integrate-state(?state:name, ?instr:expression, ?brain:expression, ?pos-table:expression) }
  =>
  {
    let next-state = ?instr.?state;
    select (next-state by instance?)
      <instruction> =>
        let pos = element(?pos-table, next-state, default: #f);
        if (pos)
          ?instr.?state := pos;
        else
          put-instruction(next-state, ?brain, ?pos-table);
          put-instruction(?instr, ?brain, ?pos-table);
        end;
      <integer> =>
        #t // we are done
    end select;
  }
end;

define method put-instruction(instr :: <sense>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  integrate-state(state-true, instr, brain, pos-table);
  integrate-state(state-false, instr, brain, pos-table);
end;
  
define method put-instruction(instr :: <mark>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  integrate-state(state, instr, brain, pos-table);
end;
  
define method put-instruction(instr :: <unmark>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  integrate-state(state, instr, brain, pos-table);
end;
  
define method put-instruction(instr :: <pickup>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  integrate-state(state-success, instr, brain, pos-table);
  integrate-state(state-failure, instr, brain, pos-table);
end;
  

define method put-instruction(instr :: <drop>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  integrate-state(state, instr, brain, pos-table);
end;

define method put-instruction(instr :: <turn>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  integrate-state(state, instr, brain, pos-table);
end;
  
define method put-instruction(instr :: <move>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  integrate-state(state-success, instr, brain, pos-table);
  integrate-state(state-failure, instr, brain, pos-table);
end;

define method put-instruction(instr :: <flip>, brain :: <stretchy-vector>, pos-table :: <table>)
 => ();
  next-method();
  integrate-state(state-success, instr, brain, pos-table);
  integrate-state(state-failure, instr, brain, pos-table);
end;
  

define function dump-brain(brain :: <vector>)
  map(compose(format-out, unparse), brain)
end;

define functional class <instruction-label-count> (<object>)
  constant slot instruction-label, required-init-keyword: label:;
  constant slot instruction-count, required-init-keyword: count:;
end;

define sealed domain make(singleton(<instruction-label-count>));
define sealed domain initialize(<instruction-label-count>);

define method functional-==
    (c == <instruction-label-count>, l :: <instruction-label-count>, r :: <instruction-label-count>)
 => (same :: <boolean>);
 format-out("functional-==: (%s, %d) ==? (%s, %d)\n", l.instruction-label, l.instruction-count, r.instruction-label, r.instruction-count);
  l.instruction-label == r.instruction-label
    & l.instruction-count == r.instruction-count
end;


// direct builders
// intended for use with "Verbatim"

define function Drop(next-state :: <instruction>)
 => instruction :: <drop>;
  make(<drop>, state: next-state)
end;






///////////// KLUDGE


define constant <continuation> = type-union(<integer>, <instruction>);

define class <instruction> (<object>)
end class <instruction>;

define class <sense> (<instruction>)
  slot sense-direction :: <sense-direction>, required-init-keyword: direction:;
  slot state-true :: <continuation>, required-init-keyword: state-true:;
  slot state-false :: <continuation>, required-init-keyword: state-false:;
  slot cond :: <ant-condition>, required-init-keyword: condition:;
end class <sense>;

define class <mark> (<instruction>)
  slot marker :: <marker>, required-init-keyword: marker:;
  slot state :: <continuation>, required-init-keyword: state:;
end class <mark>;

define class <unmark> (<instruction>)
  slot marker :: <marker>, required-init-keyword: marker:;
  slot state :: <continuation>, required-init-keyword: state:;
end class <unmark>;

define class <pickup> (<instruction>)
  slot state-success :: <continuation>, required-init-keyword: state-success:;
  slot state-failure :: <continuation>, required-init-keyword: state-failure:;
end class <pickup>;

define class <drop> (<instruction>)
  slot state :: <continuation>, required-init-keyword: state:;
end class <drop>;

define class <turn> (<instruction>)
  slot left-or-right :: <left-or-right>, required-init-keyword: left-or-right:;
  slot state :: <continuation>, required-init-keyword: state:;
end class <turn>;

define class <move> (<instruction>)
  slot state-success :: <continuation>, required-init-keyword: state-success:;
  slot state-failure :: <continuation>, required-init-keyword: state-failure:;
end class <move>;

define class <flip> (<instruction>)
  slot probability :: <integer>, required-init-keyword: probability:;
  slot state-success :: <continuation>, required-init-keyword: state-success:;
  slot state-failure :: <continuation>, required-init-keyword: state-failure:;
end class <flip>;



define generic unparse(i :: <instruction>)
 => text :: <byte-string>;

define function sense-condition-as-string(m)
 => text :: <byte-string>;
  select (m)
    Friend: => "Friend";
    Foe: => "Foe";
    FriendWithFood: => "FriendWithFood";
    FoeWithFood: => "FoeWithFood";
    Food: => "Food";
    Rock: => "Rock";
    Marker0: => "Marker 0";
    Marker1: => "Marker 1";
    Marker2: => "Marker 2";
    Marker3: => "Marker 3";
    Marker4: => "Marker 4";
    Marker5: => "Marker 5";
    FoeMarker: => "FoeMarker";
    Home: => "Home";
    FoeHome: => "FoeHome";
  end;
end;

define function sense-direction-as-string(d)
 => text :: <byte-string>;
  select (d)
    Here: => "Here";
    Ahead: => "Ahead";
    LeftAhead: => "LeftAhead";
    RightAhead: => "RightAhead";
   end;
end;

define method unparse(s :: <sense>)
 => text :: <byte-string>;
  format-to-string("Sense %s %d %d %s",
                   s.sense-direction.sense-direction-as-string,
                   s.state-true,
                   s.state-false,
                   s.cond.sense-condition-as-string);
end;


define method unparse(m :: <mark>)
 => text :: <byte-string>;
  format-to-string("Mark %d %d", m.marker, m.state);
end;

define method unparse(u :: <unmark>)
 => text :: <byte-string>;
  format-to-string("Unmark %d %d", u.marker, u.state);
end;

define method unparse(p :: <pickup>)
 => text :: <byte-string>;
  format-to-string("PickUp %d %d", p.state-success, p.state-failure);
end;

define method unparse(d :: <drop>)
 => text :: <byte-string>;
  format-to-string("Drop %d", d.state);
end;

define method unparse(t :: <turn>)
 => text :: <byte-string>;
  format-to-string("Turn %s %d", if (t.left-or-right == #"left") "Left" else "Right" end, t.state);
end;

define method unparse(m :: <move>)
 => text :: <byte-string>;
  format-to-string("Move %d %d", m.state-success, m.state-failure);
end;

define method unparse(f :: <flip>)
 => text :: <byte-string>;
  format-to-string("Flip %d %d %d", f.probability, f.state-success, f.state-failure);
end;


define constant <ant-condition> = one-of(#"friend",
                                         #"foe",
                                         #"friendwithfood",
                                         #"foewithfood",
                                         #"food",
                                         #"rock",
                                         #"marker0",
                                         #"marker1",
                                         #"marker2",
                                         #"marker3",
                                         #"marker4",
                                         #"marker5",
                                         #"foemarker",
                                         #"home",
                                         #"foehome");
define constant <marker> = limited(<integer>, min: 0, max: 5);
define constant <left-or-right> = one-of(#"left", #"right");
define constant <sense-direction> = one-of(#"Here", #"Ahead", 
                                           #"LeftAhead", #"RightAhead");

