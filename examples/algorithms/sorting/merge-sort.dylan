module:   sorting
synopsis: merge sort implementation
author:   Peter Hinely

/*
  Based on Svante Carlsson's merge sort in Sorting & Searching Experimentarium
  http://www.diku.dk/~jyrki/Experimentarium/
*/

define inline function merge
    (X :: <simple-object-vector>, B :: <simple-object-vector>,
     cursor :: <integer>, m :: <integer>, n :: <integer>, test :: <function>)
 => ()
  let k = cursor;
  let i = cursor;
  let j = m;

  //without-bounds-checks ()
  while ((k < m) & (j < n))
    let Xj = X[ j ];
    let Xk = X[ k ];
    if (test(Xj, Xk))
      B[ i ] := Xj;
      i := i + 1;
      j := j + 1;
    else
      B[ i ] := Xk;
      i := i + 1;
      k := k + 1;
    end;
  end while;
  
  if (j >= n)
    while (k < m)
      B[ i ] := X[ k ];
      i := i + 1;
      k := k + 1;
    end;
  else
    while (j < n)
      B[ i ] := X[ j ];
      i := i + 1;
      j := j + 1;
    end;
  end if;
  //end without-bounds-checks;
end function;

define inline function merge-sort!-internal
    (A :: <simple-object-vector>, sz :: <integer>, test :: <function>)
 => (sorted-vector :: <simple-object-vector>)
  let run-size :: <integer> = 1;
  let runs :: <integer> = sz;
  //let in-original :: <boolean> = #t;

  let B = make(<simple-object-vector>, size: sz);
  while (run-size < sz)
    let cursor :: <integer> = 0;
    for (i from 1 to ash(runs, -1))
      let left2 = cursor + run-size;
      let possible-right2 = left2 + run-size;
      let right2 = if (possible-right2 > sz)
                     sz;
                   else
                     possible-right2;
                   end;
      merge(A, B, cursor, left2, right2, test);  
      cursor := right2;
    end for;
    if (odd?(runs))
      for (j from cursor below sz)
        //%element(B, j) := %element(A, j);
        element(B, j) := element(A, j);
      end;
    end if;
    /* let's switch the merging areas */
    let temp = A;
    A := B;
    B := temp;
    run-size := run-size * 2;
    runs := floor/(sz, run-size) + (if (modulo(sz, run-size) > 0) 1 else 0 end);
  end while;

  A;
end function;

define function merge-sort!
    (sequence :: <sequence>, #key test :: <function> = \<)
 => (new-sequence :: <sequence>)
  let a = as(<simple-object-vector>, sequence);
  let n :: <integer> = sequence.size;

  if (n < 2)
    sequence;
  else
    as(type-for-copy(sequence), merge-sort!-internal(a, n, test));
  end if;
end function;
