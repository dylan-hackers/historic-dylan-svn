module:   sorting
synopsis: insertion sort
author:   adapted from the Gwydion Dylan source


define inline function insertion-sort!-internal
    (vec :: <simple-object-vector>,
     sz :: <integer>,
     test :: <function>)
 => (sorted-vector :: <simple-object-vector>)
  for (current-key :: <integer> from 1 below sz)
    let current-element = vec[current-key];
    for (insert-key :: <integer> from current-key - 1 to 0 by -1,
         while: test(current-element, vec[insert-key]))
      vec[insert-key + 1] := vec[insert-key];
    finally
      vec[insert-key + 1] := current-element;
    end for;
  end for;
  vec;
end function;

define function insertion-sort!
    (sequence :: <sequence>, #key test :: <function> = \<)
 => (new-sequence :: <sequence>)
  let a = as(<simple-object-vector>, sequence);
  let n :: <integer> = sequence.size;

  if (n < 2)
    sequence;
  else
    as(type-for-copy(sequence), insertion-sort!-internal(a, n, test));
  end if;
end function;

