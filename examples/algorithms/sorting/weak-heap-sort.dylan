module:   sorting
synopsis: weak heap sort implementation
author:   Peter Hinely

/*
  WEAK-HEAP-SORT (Dutton (1993)
    as described in:

    Implementing HEAPSORT with (n logn - 0.9n) and QUICKSORT with (n logn + 0.2n) comparisons
    Stefan Edelkamp, Patrick Stiegeler
    Journal of Experimental Algorithmics (JEA) archive
    2002
*/

define inline function grandparent (j :: <integer>) => (integer :: <integer>) 
  while (even?(j))
    j := ash(j, -1);
  end;
  ash(j, -1);
end function;

define inline function merge-weak-heap (a :: <simple-object-vector>, rotated :: <byte-vector>, i :: <integer>, j :: <integer>, test :: <function>) => () 
  //without-bounds-checks ()
    let element1 = a[i];
    let element2 = a[j];
    if (test(element1, element2))
      a[i] := element2;
      a[j] := element1;
      //with-bounds-checks ()
        // for some reason if we try to use %element, it results in poor code
        element(rotated, j) := 1 - element(rotated, j);
      //end;
    end;
  //end without-bounds-checks;
end function; 

define inline function weak-heapify (a :: <simple-object-vector>, rotated :: <byte-vector>, n :: <integer>, test :: <function>) => () 
  for (j from n - 1 to 1 by -1) 
    merge-weak-heap(a, rotated, grandparent(j), j, test); 
  end; 
end function; 

define inline function merge-forest (a :: <simple-object-vector>, rotated :: <byte-vector>, m :: <integer>, test :: <function>) => () 
  let x = 1;
  local method loop ()
    let temp = (2 * x) + element(rotated, x);
    if (temp < m)
      x := temp;
      loop();
    end;
  end;

  loop();

  while (x > 0)
    merge-weak-heap(a, rotated, m, x, test);
    x := ash(x, -1);
  end;
end function;

define inline function weak-heap-sort!-internal
    (a :: <simple-object-vector>, n :: <integer>, test :: <function>)
 => (sorted-vector :: <simple-object-vector>) 
  let rotated = make(<byte-vector>, size: n); 
  weak-heapify(a, rotated, n, test); 

  for (i from n - 1 to 2 by -1) 
    merge-forest(a, rotated, i, test);
  end;

  let save = a[0];
  for (i from 0 below (n - 1))
    %element(a, i) := %element(a, i + 1);
  end;
  a[n - 1] := save;

  a;
end function;

define function weak-heap-sort!
    (sequence :: <sequence>, #key test :: <function> = \<)
 => (new-sequence :: <sequence>)
  let a = as(<simple-object-vector>, sequence);
  let n :: <integer> = sequence.size;

  if (n < 2)
    sequence;
  else
    as(type-for-copy(sequence), weak-heap-sort!-internal(a, n, test));
  end if;
end function;