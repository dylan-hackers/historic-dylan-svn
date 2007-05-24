Module: regular-expressions-test-suite
Author: Carl Gay


define function re/position (string, pattern, #rest args)
  let (#rest marks) = apply(regexp-position, string, pattern, args);
  marks
end function re/position;

define test atom-test ()
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
  check-condition("atom-B", <illegal-regexp>, re/position("", "a{m,n}"));
  check-condition("atom-C", <illegal-regexp>, re/position("", "a{m,}"));
  check-condition("atom-D", <illegal-regexp>, re/position("", "a{,n}"));
  check-condition("atom-E", <illegal-regexp>, re/position("", "a{m}"));
  check-condition("atom-F", <illegal-regexp>, re/position("", "a{,"));
  check-condition("atom-G", <illegal-regexp>, re/position("", "[a"));
  check-condition("atom-H", <illegal-regexp>, re/position("", "\\"));
  check-equal("atom-tan", "\<65>", "A");
end;


define suite regular-expressions-test-suite ()
  test atom-test;
//  test and-test;
//  test or-test;
//  test anchoring-test;
//  test quantifier-test;
end;
