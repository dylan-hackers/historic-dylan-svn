Module:    strings-tests
Synopsis:  Test suite for strings library
Author:    Carl Gay


// Note: Inputs to all mutating (!) functions are copied with copy-sequence
// before being passed to the function because string literals are supposed
// to be immutable.  Functional Developer doesn't actually enfore that yet,
// which showed up some funky bugs.


define constant fmt = format-to-string;

define library-spec strings ()
  module strings;
end library-spec strings;

define module-spec strings ()
  open generic-function capitalize (<string>, #"key", #"start", #"end") => (<string>);
  open generic-function capitalize! (<string>, #"key", #"start", #"end") => (<string>);
  function downcase! (<string>) => (<string>);
  function downcase (<string>) => (<string>);
  function upcase! (<string>) => (<string>);
  function upcase (<string>) => (<string>);
  open generic-function integer-to-digit (<integer>) => (<character>);
  open generic-function digit-to-integer (<character>) => (<integer>);

  open generic-function trim
    (<string>, #"key", #"test", #"side", #"start", #"end") => (<string>);

  open generic-function join
    (<sequence>, <string>, #"key", #"conjunction") => (<string>);

  open generic-function split
    (<string>, #"key", #"separator", #"start", #"end", #"trim?", #"max",
     #"allow-empty-strings?") => (<sequence>);

  open generic-function replace
    (<string>, <string>, <string>, #"key", #"start", #"end", #"test", #"max")
    => (<string>, <integer>);

  open generic-function greater?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);
  open generic-function less?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);
  open generic-function equal?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);

  open generic-function case-insensitive-greater?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);
  open generic-function case-insensitive-less?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);
  open generic-function case-insensitive-equal?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);

  open generic-function control? (<character>) => (<boolean>);
  open generic-function printable? (<character>) => (<boolean>);
  open generic-function graphic? (<character>) => (<boolean>);
  open generic-function lowercase? (<character>) => (<boolean>);
  open generic-function uppercase? (<character>) => (<boolean>);
  open generic-function whitespace? (<character>) => (<boolean>);
  open generic-function alphanumeric? (<character>) => (<boolean>);
  open generic-function digit? (<character>) => (<boolean>);
  open generic-function alphabetic? (<character>) => (<boolean>);

  open generic-function count-occurrances
    (<string>, <string>, #"key", #"test", #"start", #"end") => (<integer>);

  open generic-function index-of
    (<string>, <string>, #"key", #"test", #"start", #"end", #"from-end?") => (<integer>);

end module-spec strings;


define strings function-test capitalize ()
  test-output(concatenate("capitlize ", "A", "\n"));
  for (item in #(#("", ""),
                 #("x", "X"),
                 #("abc", "Abc"),
                 #("Abc", "Abc"),
                 #("one two", "One Two"),
                 #("_one,two", "_One,Two")))
    let (before, after) = apply(values, item);
    check-equal(fmt("capitalize %=", before),
                capitalize(before),
                after);
  end;
  test-output(concatenate("capitlize ", "A", "\n"));
end function-test capitalize;

define strings function-test capitalize! ()
  test-output(concatenate("capitalize! ", "A", "\n"));
  for (item in #(#("", ""),
                 #("a", "A"),
                 #("abc", "Abc"),
                 #("Abc", "Abc"),
                 #("one two", "One Two"),
                 #("_one,two", "_One,Two")))
    let (before, after) = apply(values, map(copy-sequence, item));
    check-equal(fmt("capitalize! %=", before),
                capitalize!(before),
                after);
    check-true(fmt("capitalize! %= retains identity", before),
               capitalize!(before) == before);
  end;
  test-output(concatenate("capitalize! ", "A", "\n"));
end function-test capitalize!;

define strings function-test downcase! ()
  test-output(concatenate("downcase! ", "A", "\n"));
  for (item in #(#("", ""),
                 #("a", "a"),
                 #("E", "e"),
                 #("ABC", "abc"),
                 #("ONE TWO", "one two"),
                 #("_oNe,Two", "_one,two")))
    let (before, after) = apply(values, map(copy-sequence, item));
    check-equal(fmt("downcase! %=", before), downcase!(before), after);
    check-true(fmt("downcase! %= retains identity", before),
               downcase!(before) == before);
  end;
  test-output(concatenate("downcase! ", "A", "\n"));
end function-test downcase!;

define strings function-test downcase ()
  test-output(concatenate("downcase ", "A", "\n"));
  for (item in #(#("", ""),
                 #("a", "a"),
                 #("A", "a"),
                 #("ABC", "abc"),
                 #("ONE TWO", "one two"),
                 #("_oNe,Two", "_one,two")))
    let (before, after) = apply(values, item);
    check-equal(fmt("downcase %=", before), downcase(before), after);
  end;
  test-output(concatenate("downcase ", "A", "\n"));
end function-test downcase;

define strings function-test upcase! ()
  test-output(concatenate("upcase! ", "A", "\n"));
  for (item in #(#("", ""),
                 #("A", "A"),
                 #("a", "A"),
                 #("abc", "ABC"),
                 #("one two", "ONE TWO"),
                 #("_oNe,Two", "_ONE,TWO")))
    let (before, after) = apply(values, map(copy-sequence, item));
    check-equal(fmt("upcase! %=", before), upcase!(before), after);
    check-true(fmt("upcase! %= retains identity", before),
               upcase!(before) == before);
  end;
  test-output(concatenate("upcase! ", "A", "\n"));
end function-test upcase!;

define strings function-test upcase ()
  test-output(concatenate("upcase ", "A", "\n"));
  for (item in #(#("", ""),
                 #("a", "A"),
                 #("A", "A"),
                 #("abc", "ABC"),
                 #("one two", "ONE TWO"),
                 #("_oNe,Two", "_ONE,TWO")))
    let (before, after) = apply(values, item);
    check-equal(fmt("upcase %=", before), upcase(before), after);
  end;
  test-output(concatenate("upcase ", "A", "\n"));
end function-test upcase;

define strings function-test trim ()
  for (item in list(#("", ""),
                    #("a", "a"),
                    #("a", " a "),
                    #("a", " a ", side:, #"both"),  // same
                    #("a ", " a ", side:, #"left"),
                    #(" a", " a ", side:, #"right"),
                    list(" a ", " a ", test:, method (c) #f end),
                    list("o", "xox", test:, method (c) c == 'x' end)
                    ))
    let (after, before, #rest trim-args) = apply(values, item);
    check-equal(fmt("trim %=", before),
                apply(trim, before, trim-args),
                after);
  end for;
end function-test trim;

define strings function-test join ()
  for (item in list(list(list("", "-"), ""),
                    list(list(#("a", "b", "c"), "-"), "a-b-c"),
                    list(list(#("a", "b", "c"), ", ", conjunction:, " and "), "a, b and c"),
                    list(list("abc", ", ", conjunction:, " and ", key:, upcase), "A, B and C")))
    let (join-args, expected-result) = apply(values, item);
    let test-name = fmt("join(%s)",
                        join(join-args, ", ", key: method (x) fmt("%=", x) end));
    check-equal(test-name, apply(join, join-args), expected-result);
  end for;
end function-test join;

define strings function-test integer-to-digit ()
  
end function-test integer-to-digit;

define strings function-test digit-to-integer ()
  //---*** Fill this in...
end function-test digit-to-integer;

define strings function-test split ()
    check-equal("empty string", "", split(""));
    check-equal("", #["a", "b", "c"], split("a b c"));
end function-test split;

define strings function-test replace ()
  //---*** Fill this in...
end function-test replace;

define strings function-test less? ()
  //---*** Fill this in...
end function-test less?;

define strings function-test equal? ()
  //---*** Fill this in...
end function-test equal?;

define strings function-test control? ()
  //---*** Fill this in...
end function-test control?;

define strings function-test printable? ()
  //---*** Fill this in...
end function-test printable?;

define strings function-test graphic? ()
  //---*** Fill this in...
end function-test graphic?;

define strings function-test lowercase? ()
  //---*** Fill this in...
end function-test lowercase?;

define strings function-test uppercase? ()
  //---*** Fill this in...
end function-test uppercase?;

define strings function-test whitespace? ()
  //---*** Fill this in...
end function-test whitespace?;

define strings function-test alphanumeric? ()
  //---*** Fill this in...
end function-test alphanumeric?;

define strings function-test digit? ()
  //---*** Fill this in...
end function-test digit?;

define strings function-test alphabetic? ()
  //---*** Fill this in...
end function-test alphabetic?;

define strings function-test count-occurrances ()
  //---*** Fill this in...
end function-test count-occurrances;

define strings function-test index-of ()
  //---*** Fill this in...
end function-test index-of;

define strings function-test greater? ()
  //---*** Fill this in...
end function-test greater?;

define strings function-test case-insensitive-equal? ()
  //---*** Fill this in...
end function-test case-insensitive-equal?;

define strings function-test case-insensitive-less? ()
  //---*** Fill this in...
end function-test case-insensitive-less?;

define strings function-test case-insensitive-greater? ()
  //---*** Fill this in...
end function-test case-insensitive-greater?;


define method main () => ()
  let args = application-arguments();
  let debug? = #f;
  if (args.size >= 1)
    if (equal?(args[0], "--debug", test: case-insensitive-equal?))
      debug? := #t;
    end;
  end;
  perform-suite(strings-test-suite, debug?: debug?);
end method main;

begin
  main();
end;

