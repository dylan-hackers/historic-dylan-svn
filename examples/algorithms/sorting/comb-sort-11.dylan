module:   sorting
synopsis: comb sort 11.
author:   Peter Hinely

/*
  See http://cs.clackamas.cc.or.us/molatore/cs260Spr03/combsort.htm
  for more info
*/

define inline function next-gap (current-gap :: <integer>) => (gap :: <integer>)
  if (current-gap < 3)
    1;
  elseif ((current-gap > 11) & (current-gap < 15))
    11;
  else
    floor/((current-gap * 10), 13);
  end;
end;

define inline function comb-sort-11!-internal
    (data :: <simple-vector>, sz :: <integer>, test :: <function>)
 => (sorted-vector :: <simple-vector>)
  local method scan (previous-gap :: <integer>)
    let gap = next-gap(previous-gap);
    let swapped = #f;
    for (i from 0 below (sz - gap))
      let j = i + gap;
      let elt1 = data[i];
      let elt2 = data[j];
      if (test(elt2, elt1))
        data[i] := elt2;
        data[j] := elt1;
        swapped := #t;
      end;
    end;
    if ((gap > 1) | swapped)
      scan(gap);
    end;
  end;

  scan(sz);
  data;
end function;

define function comb-sort-11!
    (sequence :: <sequence>, #key test :: <function> = \<)
 => (new-sequence :: <sequence>)
  let a = as(<simple-object-vector>, sequence);
  let n :: <integer> = sequence.size;

  if (n < 2)
    sequence;
  else
    as(type-for-copy(sequence), comb-sort-11!-internal(a, n, test));
  end if;
end function;