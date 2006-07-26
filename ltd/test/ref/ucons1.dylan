//  -*- mode:common-lisp; package: mma; -*-
// LTD: Function PROVIDE not yet implemented.
provide(#"ucons1");

#f;

// LTD: Function LOAD not yet implemented.
load("hash.fasl");

//  (c) 1990, 1991, Richard J. Fateman
//  (c) 1994 Richard J. Fateman
"(in-package mma)";

//  alternative to ucons1 file of 1990, 91. using new hash table extensions
//  in Allegro 4.2 ++ (must have patch file hash.fasl installed)
//  non-standard hash table feature used below
// LTD: No macros.
#"eq-hash";

define method car-cdr-eq (key1, type, #key key2)
  if (type)
    //  this is the hash-code for a single cons
    logxor(eq-hash(head(key1)), eq-hash(tail(key1)));
  else
    //  this is the test to see if two conses have eq cars and eq cdrs
    eq-hash(head(key1)) == eq-hash(head(key2))
     & eq-hash(tail(key1)) == eq-hash(tail(key2));
  end if;
end method car-cdr-eq;

define variable *uniq-table* = make(<table>, test: car-cdr-eq);

define variable *uniq-atom-table* = make(<table>, test: \==);

define method uniq (x)
  // Return a canonical representation that is EQUAL to x,
  //   such that (equal x y) => (eq (uniq x) (uniq y))
  select (x by instance?)
    fixnum | symbol
       => x;
    atom
       => *uniq-atom-table*[x] | (*uniq-atom-table*[x] := x);
    cons
       => ucons(uniq(head(x)),
                //  this could check in 
                //  *uniq-table* first...
                uniq(tail(x)));
  end select;
end method uniq;

define variable *fakecons* = #(#"car" . #"cdr");

define method ucons (x, y)
  // Unique cons: (eq (ucons x y) (ucons x y)) is always true.
  let temp = *fakecons*;
  let tt = *uniq-table*;
  begin head(temp) := x; tail(temp) := y; end;
  let _that = #f;
  if (_that := tt[temp])
    _that;
    // If already there, great.
    else
    tt[temp] := temp;
    *fakecons* := pair(#"car", #"cdr");
    temp;
  end if;
end method ucons;

define method umapcar (f, x)
  if (empty?(x)) #f; else ucons(f(head(x)), umapcar(f, tail(x))); end if;
end method umapcar;

// LTD: No macros.
#"ulist";

define method uappend (r, s)
  if (empty?(r)) s; else ucons(head(r), uappend(tail(r), s)); end if;
end method uappend;

