Module: new-api-test-suite
Author: Carl Gay


define test groups-test ()
  let text = "My dog has fleas.";
  let match = regexp-search(text, "My (dog)");

  // The entire match is a group as well, so 2 groups.
  check-equal("number of groups.", 2, regexp-match-groups(match).size);

  let (txt, bpos, epos) = regexp-match-group(match, 1);
  check-equal("group text", "dog", txt);
  check-equal("group start", 3, bpos);
  check-equal("group end", 6, epos);
end test groups-test;

define suite new-api-test-suite ()
  test groups-test;
//  test and-test;
//  test or-test;
//  test anchoring-test;
//  test quantifier-test;
end suite new-api-test-suite;
