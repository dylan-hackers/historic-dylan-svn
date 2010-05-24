module: dylan-rep
synopsis: Representation of Dylan macro definitions.


define class <macro-binding> (<binding>)
   slot explicit-defn :: false-or(<explicit-macro-defn>) = #f,
      init-keyword: #"explicit";
end class;


define abstract class <explicit-macro-defn> (<implicit/explicit-defn>)
   // slot main-rules = make(<stretchy-vector> /* of <rule> */);
   // slot aux-rules = make(<stretchy-vector> /* of <aux-rules> */);
end class;


define class <explicit-body-macro-defn> (<explicit-macro-defn>)
end class;


define class <explicit-list-macro-defn> (<explicit-macro-defn>)
end class;


define class <explicit-stmt-macro-defn> (<explicit-macro-defn>)
end class;


define class <explicit-func-macro-defn> (<explicit-macro-defn>)
end class;


// (unused)
// define class <aux-rules> (<api-object>)
//    slot symbol :: <string>;
//    slot rules = make(<stretchy-vector> /* of <rule> */);
// end class;
// 
// define class <rule> (<api-object>)
//    slot pattern :: <pattern>;
//    slot template :: <template>;
// end class;
// 
// 
// //
// // Patterns
// //
// 
// define class <pattern> (<api-object>)
//    slot pattern-lists = make(<stretchy-vector> /* of <pattern-list> */);
// end class;
// 
// define class <pattern-list> (<api-object>)
//    slot pattern-sequences = make(<stretchy-vector> /* of <pattern-sequence> */);
//    slot property-list-pattern :: false-or(<property-list-pattern>);
// end class;
// 
// define class <pattern-sequence> (<api-object>)
//    slot simple-patterns = make(<stretchy-vector> /* of <simple-pattern> */);
// end class;
// 
// define abstract class <simple-pattern> (<api-object>)
// end class;
// 
// define class <name-not-end-token> (<simple-pattern>)
//    slot name :: <string>;
// end class;
// 
// define class <arrow-token> (<simple-pattern>)
// end class;
// 
// define class <bracketed-pattern> (<simple-pattern>)
//    slot opening-character :: <character>;
//    slot pattern :: <pattern>;
//    slot closing-character :: <character>;
// end class;
// 
// define abstract class <binding-pattern> (<simple-pattern>)
// end class;
// 
// define class <variable-pattern> (<binding-pattern>)
//    slot name :: <pattern-variable>;
//    slot type :: <pattern-variable>;
// end class;
// 
// define class <assignment-pattern> (<binding-pattern>)
//    slot variable :: <pattern-variable>;
//    slot value :: <pattern-variable>;
// end class;
// 
// define class <var-assign-pattern> (<binding-pattern>)
//    slot name :: <pattern-variable>;
//    slot type :: <pattern-variable>;
//    slot value :: <pattern-variable>;
// end class;
// 
// define abstract class <pattern-variable> (<simple-pattern>)
// end class;
// 
// define class <constrained-name-patvar> (<pattern-variable>)
//    slot name :: <string>;
//    slot constraint :: <string>;
//    slot template :: false-or(<template>);
// end class;
// 
// define class <ellipsis-patvar> (<pattern-variable>)
// end class;
// 
// define abstract class <property-list-pattern> (<api-object>)
// end class;
// 
// define class <rest-pattern> (<property-list-pattern>)
//    slot patvar :: <pattern-variable>;
// end class;
// 
// define class <key-pattern> (<property-list-pattern>)
//    slot patvar-keys = make(<stretchy-vector> /* of <pattern-keyword> */);
//    slot patvar-all-keys? :: <boolean>;
// end class;
// 
// define class <rest-key-pattern> (<property-list-pattern>)
//    slot patvar :: <pattern-variable>;
//    slot patvar-keys = make(<stretchy-vector> /* of <pattern-keyword> */);
//    slot patvar-all-keys? :: <boolean>;
// end class;
// 
// define abstract class <pattern-keyword> (<api-object>)
//    slot name :: <string>;
//    slot constraint :: <string>;
//    slot template :: false-or(<template>);
// end class;
// 
// define class <first-pattern-keyword> (<pattern-keyword>)
// end class;
// 
// define class <every-pattern-keyword> (<pattern-keyword>)
// end class;
// 
// 
// //
// // Templates
// //
// 
// define class <template> (<api-object>)
//    slot content :: <string>;
// end class;


