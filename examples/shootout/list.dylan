module:        list
synopsis:      implementation of "List Processing" benchmark
author:        Peter Hinely
copyright:     public domain


define function test-lists (items :: <integer>)
  let L1 = make(<deque>, capacity: items);
  for (i from 1 to items)
    push-last(L1, i)
  end;

  let L2 = copy-sequence(L1);

  let L3 = make(<deque>, capacity: items);
  until (L2.empty?)
    push-last(L3, L2.pop);
  end;

  until (L3.empty?)
    push-last(L2, L3.pop-last);
  end;

  L1 := reverse!(L1);

  L1[0] == items
    & every?(method(a :: <integer>, b :: <integer>) a == b end, L1, L2)
    & L1.size
end;


define function main ()
  let iters = string-to-integer(element(application-arguments(), 0, default: "1"));
  let result = #f;
  for (i from 1 to iters)
    result := test-lists(10000)
  end;
  format-out("%=\n", result);
end;


main();
