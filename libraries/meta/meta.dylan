module: meta
synopsis: exports other modules and provides common scan functions
author:  Douglas M. Auclair
copyright: (c) 2001, LGPL

define meta s(c)
  type(<space>, c), loop(type(<space>, c))
end meta s;

// a word is delimited by spaces, <>, {}, ',' or []
define collector word(w)
  loop([type(<any-char>, w), do(collect(w))])
end collector word;

define collector int(i) => (as(<string>, str).string-to-integer)
  loop([type(<digit>, i), do(collect(i))])
end collector int;

define collector number(n) => (as(<string>, str).string-to-number)
  loop([type(<num-char>, n), do(collect(n))])
end collector number;

define function string-to-number(s :: <string>) => (n :: <number>)
  let(num, idx) = s.string-to-integer;
  unless(idx >= s.size)
    let fract = copy-sequence(s, start: idx + 1);
    let rem = (fract.string-to-integer * 1.0) / (10.0 ^ fract.size);
    num := num + rem;
  end unless;
  num;
end function string-to-number;

