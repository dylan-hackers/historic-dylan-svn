Module:    strings-test-suite
Synopsis:  Test suite for strings library
Author:    Carl Gay


// Note: Inputs to all mutating (!) functions are copied with copy-sequence
// before being passed to the function because string literals are supposed to
// be immutable.  Open Dylan doesn't actually enforce that yet, which showed up
// some funky bugs.  (todo -- Reverify this statement.)


define constant fmt = format-to-string;

define library-spec strings ()
  module strings;
end library-spec strings;

define module-spec strings ()
  sealed generic-function byte-string? (<object>) => (<boolean>);

  open generic-function alphabetic?   (<character>) => (<boolean>);
  open generic-function digit?        (<character>) => (<boolean>);
  open generic-function alphanumeric? (<character>) => (<boolean>);
  open generic-function whitespace?   (<character>) => (<boolean>);
  open generic-function uppercase?    (<character>) => (<boolean>);
  open generic-function lowercase?    (<character>) => (<boolean>);
  open generic-function graphic?      (<character>) => (<boolean>);
  open generic-function printable?    (<character>) => (<boolean>);
  open generic-function control?      (<character>) => (<boolean>);


  open generic-function equal?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);
  open generic-function less?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);
  open generic-function greater?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);

  open generic-function case-insensitive-equal?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);
  open generic-function case-insensitive-less?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);
  open generic-function case-insensitive-greater?
    (<string>, <string>, #"key", #"start1", #"start2", #"end1", #"end2" #"test")
    => (<boolean>);

  open generic-function index-of
    (<string>, <string>, #"key", #"test", #"start", #"end", #"from-end?") => (<integer>);
  open generic-function count-matches
    (<string>, <string>, #"key", #"test", #"start", #"end") => (<integer>);


  open generic-function join
    (<sequence>, <string>, #"key", #"conjunction") => (<string>);

  function split
    (<string>, #"key", #"separator", #"start", #"end", #"count") => (<sequence>);

  open generic-function trim
    (<string>, #"key", #"test", #"side", #"start", #"end") => (<string>);

  open generic-function replace
    (<string>, <string>, <string>, #"key", #"start", #"end", #"test", #"max")
    => (<string>, <integer>);

  open generic-function replace!
    (<string>, <string>, <string>, #"key", #"start", #"end", #"test", #"max")
    => (<string>, <integer>);

  open generic-function uppercase  (<string>) => (<string>);
  open generic-function uppercase! (<string>) => (<string>);
  open generic-function lowercase  (<string>) => (<string>);
  open generic-function lowercase! (<string>) => (<string>);
  open generic-function capitalize  (<string>, #"key", #"start", #"end") => (<string>);
  open generic-function capitalize! (<string>, #"key", #"start", #"end") => (<string>);
  open generic-function pluralize (<string>, #"key", #"count") => (<string>);
  open generic-function a-or-an (<string>) => (<string>);


  open generic-function integer-to-digit (<integer>) => (<character>);
  open generic-function digit-to-integer (<character>) => (<integer>);

end module-spec strings;


define strings function-test byte-string? ()
  for (item in list(#("", #t),
                    #(5, #f),
                    list(as(<unicode-string>, ""), #f)))
    let (val, expected-result) = apply(values, item);
    check-equal(fmt("byte-string?(%=) => %=", val, expected-result),
                byte-string?(val),
                expected-result);
  end for;
end function-test byte-string?;


define strings function-test capitalize ()
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
end function-test capitalize;

define strings function-test capitalize! ()
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
end function-test capitalize!;

define strings function-test pluralize ()
  //---*** Fill this in...
end function-test pluralize;

define strings function-test a-or-an ()
  //---*** Fill this in...
end function-test a-or-an;

define strings function-test lowercase! ()
  for (item in #(#("", ""),
                 #("a", "a"),
                 #("E", "e"),
                 #("ABC", "abc"),
                 #("ONE TWO", "one two"),
                 #("_oNe,Two", "_one,two")))
    let (before, after) = apply(values, map(copy-sequence, item));
    check-equal(fmt("lowercase! %=", before), lowercase!(before), after);
    check-true(fmt("lowercase! %= retains identity", before),
               lowercase!(before) == before);
  end;
end function-test lowercase!;

define strings function-test lowercase ()
  for (item in #(#("", ""),
                 #("a", "a"),
                 #("A", "a"),
                 #("ABC", "abc"),
                 #("ONE TWO", "one two"),
                 #("_oNe,Two", "_one,two")))
    let (before, after) = apply(values, item);
    check-equal(fmt("lowercase %=", before), lowercase(before), after);
  end;
end function-test lowercase;

define strings function-test uppercase! ()
  for (item in #(#("", ""),
                 #("A", "A"),
                 #("a", "A"),
                 #("abc", "ABC"),
                 #("one two", "ONE TWO"),
                 #("_oNe,Two", "_ONE,TWO")))
    let (before, after) = apply(values, map(copy-sequence, item));
    check-equal(fmt("uppercase! %=", before), uppercase!(before), after);
    check-true(fmt("uppercase! %= retains identity", before),
               uppercase!(before) == before);
  end;
end function-test uppercase!;

define strings function-test uppercase ()
  for (item in #(#("", ""),
                 #("a", "A"),
                 #("A", "A"),
                 #("abc", "ABC"),
                 #("one two", "ONE TWO"),
                 #("_oNe,Two", "_ONE,TWO")))
    let (before, after) = apply(values, item);
    check-equal(fmt("uppercase %=", before), uppercase(before), after);
  end;
end function-test uppercase;

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
                    list(list(#("a", "b", "c"), ", ", conjunction:, " and "),
                         "a, b and c"),
                    list(list("abc", ", ", conjunction:, " and ", key:, uppercase),
                         "A, B and C")))
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
  // Tests for basic functionality with no keyword args
  check-equal("split empty string with another string",
              split("", "-"),
              #[""]);
  check-equal("split empty sequence",
              split(#(), #t),
              #[#()]);
  check-equal("basic split on string separator",
              split("a b c", " "),
              #["a", "b", "c"]);
  check-equal("basic split on object separator",
              split("a b c", ' '),
              #["a", "b", "c"]);
  check-equal("back-to-back separators",
              split("a  b", ' '),
              #["a", "", "b"]);
  check-equal("separators on the ends",
              split(" x ", ' '),
              #["", "x", ""]);

  // Tests for the count argument.
  check-equal("basic count test",
              split("a,b,c,d", ',', count: 1),
              #["a", "b,c,d"]);
  check-equal("basic count test",
              split("a,b,c,d", ',', count: 2),
              #["a", "b", "c,d"]);

  // Tests for the start and end arguments
  check-equal("basic start/end test",
              split("a b c d", ' ', start: 1, end: 6),
              #["", "b", "c", ""]);
  check-equal("basic start/end test",
              split("a b c d", ' ', start: 1),
              #["", "b", "c", "d"]);
end function-test split;

define function replacement-test (mutating?)
  for (item in list(list("", "", "", "", #[]),
                    list("", "", "", "replacement", #[]),
                    list("b", "a", "a", "b", #[]),
                    list("a", "a", "a", "b", #[#"end", 0]),
                    list("ac", "abc", "b", "", #[]),
                    list("axxc", "abc", "b", "xx", #[]),
                    list("abc", "abc", "b", "xx", #[#"start", 2])
                    /*
                    list("abc", "abc", "b", "xx", #[#"end", 1]),
                    list("xxa", "aaa", "a", "x", #[#"max", 2]),
                    list("AxA", "AaA", "a", "x", #[]),
                    list("xxx", "AaA", "a", "x", vector(#"test", case-insensitive-equal?))
                      */
                      ))
    let (expected, original, pattern, replacement, kwargs) = apply(values, item);
    let original = copy-sequence(original);
    let test-name = fmt("replace%s(%=, %=, %=, %=)",
                        if (mutating?) "!" else "" end,
                        original, pattern, replacement, kwargs);
    let result = block ()
                   apply(replace!, original, pattern, replacement, kwargs);
                 exception (e :: <error>)
                   #f
                 end;
    check-equal(test-name, original, result);
    if (pattern.size == replacement.size)
      check-true(fmt("%s identity", test-name), result == original);
    end;
  end for;
end function replacement-test;

define strings function-test replace ()
  replacement-test(#f);
end function-test replace;

define strings function-test replace! ()
  replacement-test(#t);
end function-test replace!;

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

define strings function-test count-matches ()
  //---*** Fill this in...
end function-test count-matches;

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

define strings function-test substring ()
  //---*** Fill this in...
end function-test substring;


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

