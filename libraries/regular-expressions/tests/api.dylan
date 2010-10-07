Module: regular-expressions-test-suite

define library-spec regular-expressions-api ()
  module regular-expressions;
end library-spec regular-expressions-api;

define module-spec regular-expressions ()
  sealed instantiable class <regex> (<mark>);
  sealed instantiable class <regex-error> (<format-string-condition>, <error>);
  sealed instantiable class <invalid-regex> (<regex-error>);
  sealed instantiable class <invalid-match-group> (<regex-error>);
  sealed instantiable class <match-group> (<object>);
  sealed instantiable class <regex-match> (<object>);

  // Compiling and accessing regex info
  sealed generic-function compile-regex
      (<string>, #"key", #"case-sensitive", #"dot-matches-all", #"verbose", #"multi-line")
      => (<regex>);
  sealed generic-function regex-pattern (<regex>) => (<string>);
  sealed generic-function regex-group-count
      (<regex>) => (<integer>);

  // Search and replace
  sealed generic-function regex-position
      (<regex>, <string>, #"key", #"start", #"end", #"case-sensitive")
      => (false-or(<string>), #"rest");
  sealed generic-function regex-replace
      (<string>, <regex>, <string>, #"key", #"start", #"end", #"case-sensitive", #"count")
      => (<string>);
  sealed generic-function regex-search
      (<regex>, <string>, #"key", #"anchored", #"start", #"end")
      => (false-or(<regex-match>));
  sealed generic-function regex-search-strings
      (<regex>, <string>, #"key", #"anchored", #"start", #"end")
      => (false-or(<regex-match>));

  // Accessing match groups
  sealed generic-function groups-by-position
      (<regex-match>) => (<sequence>);
  sealed generic-function groups-by-name
      (<regex-match>) => (<sequence>);
  sealed generic-function match-group
      (<regex-match>) => (false-or(<string>), false-or(<integer>), false-or(<integer>));

  // Accessing individual group data
  sealed generic-function group-text
      (<match-group>) => (false-or(<string>));
  sealed generic-function group-end
      (<match-group>) => (false-or(<integer>));
  sealed generic-function group-start
      (<match-group>) => (false-or(<integer>));
end module-spec regular-expressions;

define regular-expressions function-test regex-position ()
  check-no-errors("regex-position with a regex regex",
                  regex-position(compile-regex("pattern"), "pattern"));
  local method check-pos
          (test-name :: <string>, regex :: <string>, big :: <string>,
           positions :: <vector>, #rest args)
    check-equal(test-name,
                positions,
                begin
                  let regex = compile-regex(regex);
                  let (#rest marks) = apply(regex-position, regex, big, args);
                  marks
                end);
  end method check-pos;

  check-pos("pos test #1", "a*", "aaaaaaaaaa", #[0, 10]);
  check-pos("pos test #2", "a*", "aaaaabaaaa", #[0, 5]);
  check-pos("pos test #3", "ab*(cd|e)", "acd", #[0, 3, 1, 3]);
  check-pos("pos test #4", "ab*(cd|e)", "abbbbe", #[0, 6, 5, 6]);
  check-pos("pos test #5", "ab*(cd|e)", "ab", #[#f]);

  check-pos("pos test #6", "^a$", "aaaaaaaaaaaaaa", #[#f]);
  check-pos("pos test #7", "^a$", "a", #[0, 1]);
  check-pos("pos test #8", "(^a$)|aba", "abba", #[#f]);
  check-pos("pos test #9", "(^a$)|aba", "aba", #[0, 3, #f, #f]);

  check-pos("pos test #a",
            "\\bthe rain (in){1,5} spain$",
            "the rain in spain", 
            #[0, 17, 9, 11]);
  check-pos("pos test #b",
            "\\bthe rain (in){1,5} spain$",
            "the rain spain",
            #[#f]);
  check-pos("pos test #c",
            "\\bthe rain (in){1,5} spain$",
            "the rain ininin spain",
            #[0, 21, 13, 15]);
  check-pos("pos test #d",
            "\\bthe rain (in){1,5} spain$", 
            "bork the rain in spain",
            #[5, 22, 14, 16]);
  check-pos("pos test #e",
            "\\bthe rain (in){1,5} spain$",
            "the rain in spainland",
            #[#f]);
  check-pos("pos test #f",
            "\\bthe rain (in){1,5} spain$",
            "blathe rain in spain",
            #[#f]);
  check-pos("pos test #g",
            "\\bthe rain (in){1,5} spain$",
            "the rain ininininin spain",
            #[0, 25, 17, 19]);
  check-pos("pos test #h",
            "\\bthe rain (in){1,5} spain$",
            "the rain inininininin spain",
            #[#f]);
  check-pos("pos test #i", "a*", "aaaaa", #[0, 5]);
  check-pos("pos test #j", "a*", "a", #[0, 1]);
  check-pos("pos test #k", "a*", "", #[0, 0]);
  check-pos("pos test #L", "bba*c", "bbc", #[0, 3]);
  check-pos("pos test #m", "a", "bbbb", #[#f]);
  check-pos("pos test #n", "a*", "aaaaa", #[3, 4], start: 3, end: 4);
  check-pos("pos test #o", "^a*", "aaaaa", #[2, 5], start: 2);
  check-pos("pos test #p", "^a*", "baaaaa", #[2, 6], start: 2);
  check-pos("pos test #q", "^a+", "bbbaaaaa", #[#f], start: 2);
  check-pos("pos test #r", "a+", "AAaAA", #[0, 5], case-sensitive: #f);
  check-pos("pos test #s", "a+", "AAaAA", #[2, 3]);
  check-pos("pos test #t", "[a-f]+", "SdFbIeNvI", #[1, 2]);
  // This one is failing due to bug 7371
  check-pos("pos test #u", "[a-f]+", "SdFbIeNvI", #[1, 4], case-sensitve: #f);
  check-pos("pos test #v", "[\\s\\]]+", "blah[   \t]", #[5, 10]);

  // test escaped characters
  check-pos("pos test #w", "\\\"", "\\\"", #[1, 2]);
  check-pos("pos test #x", "\\\\\"", "\\\"", #[0, 2]);
  check-condition("pos test #y",
                  <invalid-regex>,
                  compile-regex("((a*)|(b*))*c"));
end function-test regex-position;

define regular-expressions function-test regex-replace ()
  let big-string = "The rain in spain and some other text";
  check-no-errors("regex-replace with regex pattern",
                  regex-replace(big-string,
                                compile-regex("the (.*) in (\\w*\\b)"),
                                "\\2 has its \\1"));
  check-equal("regex-replace #1",
              regex-replace("a or b", compile-regex("(o)(r)"), "\\2\\1"),
              "a ro b");
  check-equal("regex-replace #2",
              regex-replace(big-string, compile-regex("in"), "out"),
              "The raout out spaout and some other text");
  check-equal("regex-replace #3",
              regex-replace(big-string, compile-regex("in"), "out", count: 2),
              "The raout out spain and some other text");
  check-equal("regex-replace #4",
              regex-replace(big-string, compile-regex("in"), "out", start: 8, end: 15),
              "The rain out spain and some other text");
end function-test regex-replace;

define regular-expressions function-test regex-group-count ()
  //---*** Fill this in...
end function-test regex-group-count;

define regular-expressions function-test regex-search ()
  // Test case-sensitive parameter
  // See bug 7371
  check-true("regex-search(..., case-sensitive: #f) works for character sets",
             regex-search(compile-regex("[a-z]"), "A", case-sensitive: #f));
  check-true("regex-search(..., case-sensitive: #t) works on character sets",
             regex-search(compile-regex("[a-z]"), "A", case-sensitive: #f));
  check-false("case-sensitive: #t works for regular strings",
              regex-search(compile-regex("abc"), "aBc", case-sensitive: #t));
  check-true("case-sensitive: #f works for regular strings",
             regex-search(compile-regex("abc"), "ABC", case-sensitive: #f));
end function-test regex-search;

define regular-expressions function-test compile-regex ()
  // Test caching
  check-true("use-cache: #t uses the cache",
             compile-regex("abc") == compile-regex("abc", use-cache: #t));
  check-true("use-cache: #f doesn't use the cache",
             compile-regex("abc") ~== compile-regex("abc", use-cache: #f));

  // Test case-sensitive parameter
  // Test verbose parameter
  // Test multi-line parameter
  // Test dot-matches-all parameter
end function-test compile-regex;

define regular-expressions class-test <regex> ()
  //---*** Fill this in...
end class-test <regex>;

define sideways method make-test-instance
    (class == <regex>) => (regex :: <regex>)
  compile-regex("foo")
end;

define regular-expressions class-test <regex-error> ()
  //---*** Fill this in...
end class-test <regex-error>;

define regular-expressions class-test <invalid-regex> ()
  //---*** Fill this in...
end class-test <invalid-regex>;

define sideways method make-test-instance
    (class == <invalid-regex>) => (error :: <invalid-regex>)
  make(<invalid-regex>, pattern: "[unterminated")
end;

define regular-expressions function-test regex-search-strings ()
  //---*** Fill this in...
end function-test regex-search-strings;

define regular-expressions class-test <invalid-match-group> ()
  //---*** Fill this in...
end class-test <invalid-match-group>;

define regular-expressions function-test group-text ()
  //---*** Fill this in...
end function-test group-text;

define regular-expressions function-test group-end ()
  //---*** Fill this in...
end function-test group-end;

define regular-expressions function-test group-start ()
  //---*** Fill this in...
end function-test group-start;

define regular-expressions function-test groups-by-name ()
  //---*** Fill this in...
end function-test groups-by-name;

define regular-expressions function-test groups-by-position ()
  //---*** Fill this in...
end function-test groups-by-position;

define regular-expressions function-test match-group ()
  //---*** Fill this in...
end function-test match-group;

define regular-expressions class-test <match-group> ()
  //---*** Fill this in...
end class-test <match-group>;

define sideways method make-test-instance
    (class == <match-group>) => (group :: <match-group>)
  make(<match-group>, text: "foo", start: 0, end: 3)
end;

define regular-expressions class-test <regex-match> ()
  //---*** Fill this in...
end class-test <regex-match>;

define sideways method make-test-instance
    (class == <regex-match>) => (match :: <regex-match>)
  make(<regex-match>, regular-expression: compile-regex("foo"))
end;

define regular-expressions function-test regex-pattern ()
  //---*** Fill this in...
end function-test regex-pattern;


