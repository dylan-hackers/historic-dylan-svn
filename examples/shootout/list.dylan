module:        list
synopsis:      implementation of "List Processing" benchmark
author:        Peter Hinely
copyright:     public domain
use-libraries: common-dylan, format-out, garbage-collection
use-modules:   common-dylan, format-out, garbage-collection


define function test-lists (number-of-elements :: <integer>) => result :: <integer>;
  // first create a list (L1) of integers from 1 through SIZE.
  let L1 = make(<deque>, size: number-of-elements);
  for (i from 1 to number-of-elements) L1[i - 1] := i end;

  // copy L1 to L2 (can use any builtin list copy function, if available)
  let L2 = copy-sequence(L1);

  // remove each individual item from left side (head) of L2 and append to right side (tail) of L3 (preserving order).
  // (L2 should be emptied by one item at a time as that item is appended to L3).
  let L3 = make(<deque>, capacity: L2.size);
  until (L2.empty?)
    push-last(L3, L2.pop);
  end;

  // remove each individual item from right side (tail) of L3 and append to right side (tail) of L2 (reversing list).
  // (L3 should be emptied by one item at a time as that item is appended to L2).
  until (L3.empty?)
    push-last(L2, L3.pop-last);
  end;

  // reverse L1 (preferably in place) (can use any builtin function for this, if available).
  L1 := reverse!(L1);

  // check that first item of L1 is now == SIZE.
  unless (L1[0] == number-of-elements)
    for (e keyed-by n in L1) format-out("%d: %=\n", n, e) end;
    error("Error: bad first element in L1. Exiting...\n");
  end;

  // and compare L1 and L2 for equality
  for (elt1 :: <integer> in L1, elt2 :: <integer> in L2)
    unless (elt1 == elt2)
      error("Error: the elements of L1 and L2 not equal. Exiting...\n");
    end;
  end;

  // and return length of L1 (which should be equal to SIZE).
  L1.size;
end function test-lists;


define function main () => ()
  GC-free-space-divisor() := 2;
  let arg = string-to-integer(element(application-arguments(), 0, default: "1"));
  let result = #f;
  for (i from 1 to arg)
    result := test-lists(10000)
  end;
  format-out("%d\n", result);
end function main;


main();
