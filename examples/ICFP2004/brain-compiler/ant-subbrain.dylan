module: assembler


define macro ant-subbrain-definer
  { define ant-subbrain ?:name ?ant-states end }
  =>
  {
    define function ?name(outer-instrs, label, current-counter) => ();
      let instrs = make(<table>);
      instrs[#"VARIABLES"] := outer-instrs[#"VARIABLES"]; // for now ###///shallow-copy(outer-instrs[#"VARIABLES"]);
//      push-thunk(instrs, ?#"return-name", 0, curry(lookup, outer-instrs, label, current-counter + 1));
      push-thunk(outer-instrs, label, current-counter, curry(lookup, instrs, ?#"name", 1));
      let (label, counter) = values(?#"name", 0);
      ?ant-states;
    end function
  }
  
  ant-states:
  { } => { }
  { ?ant-state; ... }
    => { let counter = counter + 1; ?ant-state; ... }

  ant-state:

  { Move ?success:expression ?failure:expression }
  =>
  {
    curry(make, <move>,
          state-success: curry(lookup, instrs, label, ?success),
          state-failure: curry(lookup, instrs, label, ?failure))
  
  }
  
  { PickUp ?success:expression ?failure:expression }
  =>
  {
    curry(make, <pickup>,
          state-success: curry(lookup, instrs, label, ?success),
          state-failure: curry(lookup, instrs, label, ?failure))
  }
  
  { Flip ?prob:expression ?success:expression ?failure:expression }
  =>
  {
    curry(make, <flip>,
          probability: ?prob,
          state-success: curry(lookup, instrs, label, ?success),
          state-failure: curry(lookup, instrs, label, ?failure))
  }
  
  { Drop ?state:expression }
  =>
  {
    curry(make, <drop>,
          state: curry(lookup, instrs, label, ?state))
  }
  
  { Turn Left ?state:expression }
  =>
  {
    curry(make, <turn>,
          left-or-right: left:,
          state: curry(lookup, instrs, label, ?state))
  }
  
  { Turn Right ?state:expression }
  =>
  {
    curry(make, <turn>,
          left-or-right: right:,
          state: curry(lookup, instrs, label, ?state))
  }
  
  { Mark ?what:expression ?state:expression }
  =>
  {
    curry(make, <mark>,
          marker: ?what,
          state: curry(lookup, instrs, label, ?state))
  }
  
  { Unmark ?what:expression ?state:expression }
  =>
  {
    curry(make, <unmark>,
          marker: ?what,
          state: curry(lookup, instrs, label, ?state))
  }
  
  { Sense ?where:name ?success:expression ?failure:expression Marker ?what:expression }
  =>
  {
    curry(make, <sense>,
          direction: ?#"where",
          condition: as(<symbol>, format-to-string("marker%d", ?what)),
          state-true: curry(lookup, instrs, label, ?success),
          state-false: curry(lookup, instrs, label, ?failure))
  }

  { Sense ?where:name ?success:expression ?failure:expression ?what:name }
  =>
  {
    curry(make, <sense>,
          direction: ?#"where",
          condition: ?#"what",
          state-true: curry(lookup, instrs, label, ?success),
          state-false: curry(lookup, instrs, label, ?failure))
  }
end macro ant-subbrain-definer;

define ant-subbrain test-ant-subbrain
   Move 3 5
;   PickUp 3 5
;   Flip 2 3 5
;   Mark 3 5
;   Unmark 3 5
;   Turn Right 5
;   Turn Left 5
;   Move 3 5
;   Sense Ahead 1 3 Food
;Sense RightAhead 21 15 Marker 1
end;
