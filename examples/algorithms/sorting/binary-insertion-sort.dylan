module:   sorting
synopsis: binary insertion sort implementation
author:   Peter Hinely


define inline function binary-insertion-sort!-internal
    (vec :: <simple-object-vector>,
     sz :: <integer>,
     test :: <function>)
 => (sorted-vector :: <simple-object-vector>)
  for (i from 1 below sz)
    let left = 0;
    let right = i;
    let current-key = vec[i];

    while (left < right)
      let middle = ash((left + right), -1);
      if (test(current-key, vec[middle]))
        right := middle;
      else
        left := middle + 1;
      end;
    end while;

    for (j from i above left by -1)
       vec[j] := vec[j - 1];
       finally vec[left] := current-key;
    end;
  end for;
  vec;
end function;

define function binary-insertion-sort!
    (sequence :: <sequence>, #key test :: <function> = \<)
 => (new-sequence :: <sequence>)
  let a = as(<simple-object-vector>, sequence);
  let n :: <integer> = sequence.size;

  if (n < 2)
    sequence;
  else
    as(type-for-copy(sequence), binary-insertion-sort!-internal(a, n, test));
  end if;
end function;
