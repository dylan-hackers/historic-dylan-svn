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


  open generic-function trim
    (<string>, #"key", #"test", #"side", #"start", #"end") => (<string>);

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
                expected-result,
                byte-string?(val));
  end for;
end function-test byte-string?;


define strings function-test capitalize ()
  for (item in #(#("", ""),
                 #("x", "X"),
                 #("abc", "Abc"),
                 #("Abc", "Abc"),
                 #("one two", "One Two"),
                 #("_one,two", "_One,Two")))
    let (before, expected) = apply(values, item);
    check-equal(fmt("capitalize %=", before),
                expected,
                capitalize(before));
  end;
end function-test capitalize;

define strings function-test capitalize! ()
  for (item in #(#("", ""),
                 #("a", "A"),
                 #("abc", "Abc"),
                 #("Abc", "Abc"),
                 #("one two", "One Two"),
                 #("_one,two", "_One,Two")))
    let (before, expected) = apply(values, map(copy-sequence, item));
    check-equal(fmt("capitalize! %=", before),
                expected,
                capitalize!(before));
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
    let (before, expected) = apply(values, map(copy-sequence, item));
    check-equal(fmt("lowercase! %=", before), expected, lowercase!(before));
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
    let (before, expected) = apply(values, item);
    check-equal(fmt("lowercase %=", before), expected, lowercase(before));
  end;
end function-test lowercase;

define strings function-test uppercase! ()
  for (item in #(#("", ""),
                 #("A", "A"),
                 #("a", "A"),
                 #("abc", "ABC"),
                 #("one two", "ONE TWO"),
                 #("_oNe,Two", "_ONE,TWO")))
    let (before, expected) = apply(values, map(copy-sequence, item));
    check-equal(fmt("uppercase! %=", before), expected, uppercase!(before));
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
    let (before, expected) = apply(values, item);
    check-equal(fmt("uppercase %=", before), expected, uppercase(before));
  end;
end function-test uppercase;

define strings function-test trim ()
  for (item in list(#("", ""),
                    #("a", "a"),
                    #("a", " a "),
                    #("a", " a ", from:, #"both"),  // same
                    #("a ", " a ", from:, #"left"),
                    #(" a", " a ", from:, #"right"),
                    list(" a ", " a ", test:, method (c) #f end),
                    list("o", "xox", test:, method (c) c == 'x' end)
                    ))
    let (expected, before, #rest trim-args) = apply(values, item);
    check-equal(fmt("trim %=", before),
                expected,
                apply(trim, before, trim-args));
  end for;
end function-test trim;

define strings function-test join ()
  let abc = #["a", "b", "c"];
  check-equal("join one element",
              "foo",
              join(#["foo"], "-"));
  check-equal("join with empty separator",
              "abc",
              join(abc, ""));
  check-equal("join with non-empty separator",
              "a-b-c",
              join(abc, "-"));
  check-equal("join with conjunction",
              "a, b and c",
              join(abc, ", ", conjunction: " and "));
  check-equal("join with conjunction and key",
              "A, B and C",
              join(abc, ", ", conjunction: " and ", key: uppercase));
  check-condition("join an empty sequence is an error",
                  <error>,
                  join(#[], "-"));
end function-test join;

define strings function-test integer-to-digit ()
  
end function-test integer-to-digit;

define strings function-test digit-to-integer ()
  //---*** Fill this in...
end function-test digit-to-integer;

define strings function-test less? ()
  //---*** Fill this in...
end function-test less?;

define strings function-test equal? ()
  local method e (s1, s2, #rest args)
          check-true(fmt("equal?(%=, %=, ,@%=)", s1, s2, args),
                     apply(equal?, s1, s2, args))
        end;
  e("", "");
  e("abc", "abc");
  e("xaaax", "yaaay", start1: 1, end1: 4, start2: 1, end2: 4);
  e("a", "", end1: 0);
  e("a", "", start1: 1);
  e("", "a", end2: 0);
  e("", "a", start2: 1);
  e("abcd", "ab", end1: 2);
  e("abcd", "ab", end1: 1, end2: 1);
  e("abcd", "bc", start1: 1, end1: 3);
  e("abcd", "cd", start1: 2);
  e("ab", "abcd", end2: 2);
  e("ab", "abcd", end1: 1, end2: 1);
  e("bc", "abcd", start2: 1, end2: 3);
  e("cd", "abcd", start2: 2);
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
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "strings-test-suite")
    run-test-application(strings-test-suite);
  end;
end method main;

begin
  main()
end;

