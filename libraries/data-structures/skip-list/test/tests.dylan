language: infix-dylan
module: skip-list-test


define constant $sample-elems =
    #["sally", "sells", "sea", "shells", "by", "the", "sea", "shore"];

define constant $sample-keys = 
    #[1, 2, 3, 4, 5, 6, 7, 8];

define function make-skip-list ()
  let skip = make(<skip-list>);
  for (k in $sample-keys, e in $sample-elems)
    skip[k] := e;
  end for;
  skip
end function;


define suite skip-list-suite ()
  test string-key-test;
  test make-test;
  test not-key-order-test;
  test remove-first-key-test;
  test remove-last-key-test;
  test remove-middle-key-test;
  test empty-test;
  test backward-iteration-test;
  test keywise-iteration-test;
  test missing-test;
  test reorder-elements-test;
  test reorder-extra-element-test;
  test reorder-missing-element-test;
  test reorder-duped-element-test;
  test reorder-changed-element-test;
end suite;


define test string-key-test ()
  let skip = make(<skip-list>);
  skip["zzz"] := 1;
  skip["aaa"] := 2;
  skip["AAA"] := 3;
  skip["rrr"] := 4;
  check-equal("key order", #["zzz", "aaa", "AAA", "rrr"], key-sequence(skip));
end test;

define test make-test ()
  let skip = make-skip-list();
  check-equal("size check", $sample-elems.size, skip.size);
  check-equal("keys check", $sample-keys, skip.key-sequence);
  check-equal("elems check", $sample-elems, skip.element-sequence);
end test;


define test not-key-order-test ()
  let keys = $sample-keys.reverse;
  let skip = make(<skip-list>);
  for (k in keys, e in $sample-elems)
    skip[k] := e;
  end for;
  check-equal("keys check", keys, skip.key-sequence);
  check-equal("elems check", $sample-elems, skip.element-sequence);
end test;


define test remove-first-key-test ()
  let skip = make-skip-list();
  remove-key!(skip, 1);
  check-equal("keys check",
      copy-sequence($sample-keys, start: 1),
      skip.key-sequence);
  check-equal("elems check",
      copy-sequence($sample-elems, start: 1),
      skip.element-sequence);
  check-true("iterate check", format-to-string("%=", skip));
end test;


define test remove-last-key-test ()
  let skip = make-skip-list();
  remove-key!(skip, 8);
  check-equal("keys check",
      copy-sequence($sample-keys, end: $sample-keys.size - 1),
      skip.key-sequence);
  check-equal("elems check",
      copy-sequence($sample-elems, end: $sample-keys.size - 1),
      skip.element-sequence);
  check-true("iterate check", format-to-string("%=", skip));
end test;


define test remove-middle-key-test ()
  let skip = make-skip-list();
  remove-key!(skip, 3);
  remove-key!(skip, 4);
  check-equal("keys check",
      #[1, 2, 5, 6, 7, 8],
      skip.key-sequence);
  check-equal("elems check",
      #["sally", "sells", "by", "the", "sea", "shore"],
      skip.element-sequence);
  check-true("iterate check", format-to-string("%=", skip));
end test;


define test empty-test ()
  let skip = make(<skip-list>);
  skip[1] := "sally";
  remove-key!(skip, 1);
  check-equal("size check", 0, skip.size);
  check-true("keys check", skip.key-sequence.empty?);
  check-true("elems check", skip.element-sequence.empty?);
  check-equal("iterate check", "{skip-list }", format-to-string("%=", skip));
end test;


define test backward-iteration-test ()
  let skip = make-skip-list();
  let keys = make(<simple-object-vector>, size: $sample-keys.size);
  let elems = make(<simple-object-vector>, size: $sample-elems.size);
  for (e keyed-by k in skip using backward-iteration-protocol,
       i from 0)
    keys[i] := k;
    elems[i] := e;
  end for;
  check-equal("keys check", $sample-keys.reverse, keys);
  check-equal("elems check", $sample-elems.reverse, elems);
end test;


define test keywise-iteration-test ()
  let skip = make-skip-list();
  let sorted = sort(skip.element-sequence, stable: #t);
  skip.element-sequence := sorted;
  let keys = make(<simple-object-vector>, size: $sample-keys.size);
  let elems = make(<simple-object-vector>, size: $sample-elems.size);
  for (e keyed-by k in skip using forward-by-key-iteration-protocol,
       i from 0)
    keys[i] := k;
    elems[i] := e;
  end for;
  check-equal("keys check", $sample-keys, keys);
  check-equal("elems check", $sample-elems, elems);
end test;


define test missing-test ()
  let skip = make-skip-list();
  check-condition("elem check", <error>, skip["felix"]);
end test;


define test reorder-elements-test ()
  let skip = make-skip-list();
  let sorted = sort(skip.element-sequence, stable: #t);
  skip.element-sequence := sorted;
  check-equal("keys check",
      #[5, 1, 3, 7, 2, 4, 8, 6],
      skip.key-sequence);
  check-equal("elems check",
      #["by", "sally", "sea", "sea", "sells", "shells", "shore", "the"],
      skip.element-sequence);
  check-true("iterate check", format-to-string("%=", skip));
end test;


define test reorder-extra-element-test ()
  let skip = make-skip-list();
  let sorted = sort(skip.element-sequence, stable: #t);
  let sorted = add!(sorted, "bogus");
  check-condition("cond check", <error>, skip.element-sequence := sorted);
end test;


define test reorder-missing-element-test ()
  let skip = make-skip-list();
  let sorted = sort(skip.element-sequence, stable: #t);
  let sorted = replace-subsequence!(sorted, #[], start: 2, end: 3);
  check-condition("cond check", <error>, skip.element-sequence := sorted);
end test;


define test reorder-duped-element-test ()
  let skip = make-skip-list();
  let sorted = sort(skip.element-sequence, stable: #t);
  sorted[0] := sorted[1];
  check-condition("cond check", <error>, skip.element-sequence := sorted);
end test;


define test reorder-changed-element-test ()
  let skip = make-skip-list();
  let sorted = sort(skip.element-sequence, stable: #t);
  sorted[3] := "bogus";
  check-condition("cond check", <error>, skip.element-sequence := sorted);
end test;


run-test-application(skip-list-suite);
