module: assembler


define macro ant-subbrain-definer
  { define returning ant-subbrain ?:name ?ant-states end }
  =>
  {
    define function ?name(outer-instrs, orig-label, current-counter) => ();
      let instrs = make(<table>);
      instrs[#"VARIABLES"] := outer-instrs[#"VARIABLES"]; // for now ###///shallow-copy(outer-instrs[#"VARIABLES"]);
      push-thunk(outer-instrs, orig-label, current-counter, curry(lookup, instrs, ?#"name", 0));

      let (label, counter) = values(?#"name", -1);
      ?ant-states;
      push-thunk(instrs, ?#"name", counter + 1, curry(lookup, outer-instrs, orig-label, current-counter + 1));
    end function
  }

  { define ant-subbrain ?:name(?outsiders) ?ant-states end }
  =>
  {
    define function ?name(outer-instrs, label, current-counter) => ();
      let instrs = make(<table>);
      instrs[#"VARIABLES"] := outer-instrs[#"VARIABLES"]; // for now ###///shallow-copy(outer-instrs[#"VARIABLES"]);
      push-thunk(outer-instrs, label, current-counter, curry(lookup, instrs, ?#"name", 0));
      ?outsiders;
      let (label, counter) = values(?#"name", -1);
      ?ant-states;
    end function
  }
  
  { define ant-subbrain ?:name ?ant-states end }
  =>
  {
    define function ?name(outer-instrs, label, current-counter) => ();
      let instrs = make(<table>);
      instrs[#"VARIABLES"] := outer-instrs[#"VARIABLES"]; // for now ###///shallow-copy(outer-instrs[#"VARIABLES"]);
      push-thunk(outer-instrs, label, current-counter, curry(lookup, instrs, ?#"name", 0));
      let (label, counter) = values(?#"name", -1);
      ?ant-states;
    end function
  }
  
  outsiders:
  { } => { }
  { ?:name, ... }
    =>
  { let "outsider_" ## ?name = curry(lookup, outer-instrs, ?#"name", 0); ... }
  
  
  ant-states:
  { } => { }
  { ?ant-state; ... }
    => { let counter = counter + 1; ?ant-state; ... }

  ant-state:

  { Move ?success:name ?failure:expression } // 2
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <move>,
                     state-success: "outsider_" ## ?success,
                     state-failure:  curry(lookup, instrs, label, ?failure)))
  }
  
  { Move ?success:expression ?failure:name } // 3
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <move>,
                     state-success: curry(lookup, instrs, label, ?success),
                     state-failure:  "outsider_" ## ?failure))
  }
  
  { Move ?success:expression ?failure:expression } // 4
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <move>,
                     state-success: curry(lookup, instrs, label, ?success),
                     state-failure: curry(lookup, instrs, label, ?failure)))
  }
  
  { PickUp ?success:expression ?failure:expression }
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <pickup>,
                     state-success: curry(lookup, instrs, label, ?success),
                     state-failure: curry(lookup, instrs, label, ?failure)))
  }
  
  { Flip ?prob:expression ?success:expression ?failure:expression }
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <flip>,
                     probability: ?prob,
                     state-success: curry(lookup, instrs, label, ?success),
                     state-failure: curry(lookup, instrs, label, ?failure)))
  }
  
  { Drop ?state:expression }
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <drop>,
                     state: curry(lookup, instrs, label, ?state)))
  }
  
  { Turn ?:name ?state:expression }
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <turn>,
                     left-or-right: ?#"name",
                     state: curry(lookup, instrs, label, ?state)))
  }
  
  { Mark ?what:expression ?state:expression }
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <mark>,
                     marker: ?what,
                     state: curry(lookup, instrs, label, ?state)))
  }
  
  { Unmark ?what:expression ?state:expression }
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <unmark>,
                     marker: ?what,
                     state: curry(lookup, instrs, label, ?state)))
  }
  
  { Sense ?where:name ?success:expression ?failure:expression Marker ?what:expression }
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <sense>,
                     direction: ?#"where",
                     condition: as(<symbol>, format-to-string("marker%d", ?what)),
                     state-true: curry(lookup, instrs, label, ?success),
                     state-false: curry(lookup, instrs, label, ?failure)))
  }

  { Sense ?where:name ?success:name ?failure:name ?what:name } // 1
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <sense>,
                     direction: ?#"where",
                     condition: ?#"what",
                     state-true: "outsider_" ## ?success,
                     state-false: "outsider_" ## ?failure))
  }

  { Sense ?where:name ?success:name ?failure:expression ?what:name } // 2
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <sense>,
                     direction: ?#"where",
                     condition: ?#"what",
                     state-true: "outsider_" ## ?success,
                     state-false: curry(lookup, instrs, label, ?failure)))
  }
  
  { Sense ?where:name ?success:expression ?failure:name ?what:name } // 3
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <sense>,
                     direction: ?#"where",
                     condition: ?#"what",
                     state-true: curry(lookup, instrs, label, ?success),
                     state-false: "outsider_" ## ?failure))
  }
  
  { Sense ?where:name ?success:expression ?failure:expression ?what:name } // 4
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <sense>,
                     direction: ?#"where",
                     condition: ?#"what",
                     state-true: curry(lookup, instrs, label, ?success),
                     state-false: curry(lookup, instrs, label, ?failure)))
  }

/*  { Sense ?where:name ?success-cont ?failure-cont ?what:name }
  =>
  {
    push-thunk(instrs, label, counter,
               curry(make, <sense>,
                     direction: ?#"where",
                     condition: ?#"what",
//                     state-true: curry(lookup, instrs, label, ?success),
//                     state-false: curry(lookup, instrs, label, ?failure)))
                     state-true: ?success-cont,
                     state-false: ?failure-cont))
  }
  
  success-cont:
  { ?:name } => { "outsider_" ## ?name }
  { ?:expression } => { curry(lookup, instrs, label, ?expression) }

  failure-cont:
  { ?:name } => { "outsider_" ## ?name }
  { ?:expression } => { curry(lookup, instrs, label, ?expression) }
  */
  
end macro ant-subbrain-definer;

