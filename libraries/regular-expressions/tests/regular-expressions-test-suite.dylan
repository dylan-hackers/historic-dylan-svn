Module: regular-expressions-test-suite
Author: Carl Gay

define function re/position (string, pattern, #rest args)
  let (#rest marks) = apply(regex-position, string, pattern, args);
  marks
end function re/position;

define test atom-test ()
  // In current code the empty string is an illegal regex, but Python
  // (and probably perl?) allow it, so I think we should consider that
  // a bug.  --cgay
  check-no-errors("atom-0", re/position("", ""));
  check-equal("atom-1", re/position("a", "a"),      #[0, 1]);
  check-equal("atom-2", re/position("a", "[a]"),    #[0, 1]);
  check-equal("atom-3", re/position("ab", "(a)b"),  #[0, 2, 0, 1]);
  check-equal("atom-4", re/position("a", "\\w"),    #[0, 1]);
  check-equal("atom-5", re/position("a", "."),      #[0, 1]);
  check-equal("atom-6", re/position("a", "a{0}"),   #[0, 0]);
  check-equal("atom-7", re/position("aa", "a{2}"),  #[0, 2]);
  check-equal("atom-8", re/position("aa", "a{1,}"), #[0, 2]);
  check-equal("atom-9", re/position("aaa", "a{1,8}"), #[0, 3]);
  check-equal("atom-A", re/position("", "a{,}"),   #[0, 0]);
  check-equal("atom-A1", re/position("aaaaaa", "a{,}"),   #[0, 6]);
  check-condition("atom-B", <invalid-regex>, re/position("", "a{m,n}"));
  check-condition("atom-C", <invalid-regex>, re/position("", "a{m,}"));
  check-condition("atom-D", <invalid-regex>, re/position("", "a{,n}"));
  check-condition("atom-E", <invalid-regex>, re/position("", "a{m}"));
  check-condition("atom-F", <invalid-regex>, re/position("", "a{,"));
  check-condition("atom-G", <invalid-regex>, re/position("", "[a"));
  check-condition("atom-H", <invalid-regex>, re/position("", "\\"));
  //check-equal("atom-tan", "\<44>\<79>\<6c>\<61>\<6e>", "Dylan");
end;

// These should all compile.
//
define test good-regex-test ()
  let patterns = #(
    "",
    "a()b",
    "a(?#blah)b"
    );
  for (pattern in patterns)
    check-no-errors(format-to-string("Regex '%s' compiles", pattern),
                    compile-regex(pattern));
  end;
end test good-regex-test;

// All these regexes should signal <invalid-regex> on compilation.
//
define test bad-regex-test ()
  let patterns = #(
    "(?P<name>x)(?P<name>y)",         // can't use same name twice
    "(?@abc)"                         // invalid extended character '@'
    );
  for (pattern in patterns)
    check-condition(format-to-string("Compiling '%s' gets an error", pattern),
                    <invalid-regex>,
                    compile-regex(pattern));
  end;
end test bad-regex-test;

define test pcre-testoutput1 ()
  run-pcre-checks(make-pcre-locator("pcre-testoutput1.txt"));
end;

define suite pcre-test-suite ()
  test pcre-testoutput1;
end;

define test regressions-test ()
  run-pcre-checks(make-pcre-locator("regression-tests.txt"));
end;

define suite regular-expressions-test-suite ()
  test atom-test;
  test good-regex-test;
  test bad-regex-test;
  test regressions-test;
  // For now leaving out the PCRE tests because they're too verbose.
  // Once more basic stuff is working will start including them again.
  // suite pcre-test-suite;
end;

