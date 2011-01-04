Module: uncommon-dylan-test-suite
Author: Carl Gay

define library-spec uncommon-dylan ()
  module uncommon-dylan;
end library-spec uncommon-dylan;

define module-spec uncommon-dylan ()
  // Tries
  open instantiable class <trie-error> (<format-string-condition>, <error>);
  open class <string-trie> (<object>);
  function find-object
    (<string-trie>, <sequence>) => (<object>, <sequence>, <sequence>);
  function trie-object (<string-trie>) => (<object>);
  function remove-object (<string-trie>, <sequence>) => ();
  function add-object (<string-trie>, <sequence>, <object>, key:, replace?:) => ();
  function trie-children (<string-trie>) => (<sequence>);

  // Conversions
  function float-to-formatted-string (<float>) => (<string>);
  function string-to-float (<string>) => (<float>);

  // Collections
  function count (<collection>, <function>, key:, limit:) => (<integer>);
  function value-sequence (<table>) => (<sequence>);
  function remove-keys (<sequence>, rest:) => (<list>);

  // Types
  constant <positive-integer> :: <object>;
  constant <nonnegative-integer> :: <object>;

  macro-test inc!-test;
  macro-test dec!-test;
  macro-test wrapping-inc!-test;

  open class <singleton-object> (<object>);
  macro-test with-simple-restart-test;
  macro-test ignore-errors-test;
  macro-test with-restart-test;
  macro-test iff-test;
  macro-test bind-test;
end module-spec uncommon-dylan;

define uncommon-dylan function-test find-object ()
  //---*** Fill this in...
end function-test find-object;

define uncommon-dylan function-test float-to-formatted-string ()
  //---*** Fill this in...
end function-test float-to-formatted-string;

define uncommon-dylan function-test trie-object ()
  //---*** Fill this in...
end function-test trie-object;

define uncommon-dylan class-test <singleton-object> ()
  //---*** Fill this in...
end class-test <singleton-object>;


define uncommon-dylan function-test value-sequence ()
  //---*** Fill this in...
end function-test value-sequence;

define uncommon-dylan function-test remove-object ()
  //---*** Fill this in...
end function-test remove-object;

define uncommon-dylan function-test string-to-float ()
  //---*** Fill this in...
end function-test string-to-float;

define uncommon-dylan macro-test with-simple-restart-test ()
  //---*** Fill this in...
end macro-test with-simple-restart-test;

define uncommon-dylan function-test add-object ()
  //---*** Fill this in...
end function-test add-object;

define uncommon-dylan class-test <trie-error> ()
  //---*** Fill this in...
end class-test <trie-error>;

define uncommon-dylan class-test <string-trie> ()
  //---*** Fill this in...
end class-test <string-trie>;

define uncommon-dylan macro-test ignore-errors-test ()
  //---*** Fill this in...
end macro-test ignore-errors-test;

define uncommon-dylan function-test count ()
  let seq = #[1, 2, 2, 3, 3, 3];
  check-equal("count(#[], curry(=, #f)) => 0", 0, count(#[], curry(\=, #f)));
  check-equal("count(seq, curry(=, #f)) => 0", 0, count(seq, curry(\=, #f)));
  check-equal("count(seq, curry(=, 1)) => 1", 1, count(seq, curry(\=, 1)));
  check-equal("count(seq, curry(=, 2)) => 2", 2, count(seq, curry(\=, 2)));
  check-equal("count(seq, curry(=, 3)) => 3", 3, count(seq, curry(\=, 3)));
  check-equal("count(seq, curry(=, 3), limit: 2) => 2",
              2, count(seq, curry(\=, 3), limit: 2));
end function-test count;

define uncommon-dylan macro-test with-restart-test ()
  //---*** Fill this in...
end macro-test with-restart-test;

define uncommon-dylan macro-test wrapping-inc!-test ()
  //---*** Fill this in...
end macro-test wrapping-inc!-test;

define uncommon-dylan function-test raise ()
  //---*** Fill this in...
end function-test raise;

define uncommon-dylan macro-test iff-test ()
  //---*** Fill this in...
end macro-test iff-test;

define uncommon-dylan macro-test dec!-test ()
  //---*** Fill this in...
end macro-test dec!-test;

define uncommon-dylan constant-test <positive-integer> ()
  //---*** Fill this in...
end constant-test <positive-integer>;

define uncommon-dylan constant-test <nonnegative-integer> ()
  //---*** Fill this in...
end constant-test <nonnegative-integer>;

define uncommon-dylan function-test remove-keys ()
  //---*** Fill this in...
end function-test remove-keys;

define uncommon-dylan macro-test bind-test ()
  //---*** Fill this in...
end macro-test bind-test;

define uncommon-dylan function-test trie-children ()
  //---*** Fill this in...
end function-test trie-children;

define uncommon-dylan macro-test inc!-test ()
  //---*** Fill this in...
end macro-test inc!-test;

define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "uncommon-dylan-test-suite")
    run-test-application(uncommon-dylan-test-suite);
  end;
  // temp bug work-around
  force-output(*standard-output*);
end method main;

main();


