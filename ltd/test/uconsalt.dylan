// LTD: Function PROVIDE not yet implemented.
provide(#"ucons1");

//  (c) 1990, 1991, Richard J. Fateman
"(in-package mma)";

//  alternative to ucons1 file
//  for non-Allegro CL.  This is a much inferior version in
//  efficiency of the unique-ification, and any CL could do
//  better. But maybe not the same way.
// Simplest way to make the substitution would be to rename this
//  file ucons1.lisp.
define variable *uniq-table* = make(<table>, test: \==);

define variable *uniq-atom-table* = make(<table>, test: \=);

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

define method ucons (x, y)
  // Unique cons: (eq (ucons x y) (ucons x y)) is always true.
  let car-table
      = *uniq-table*[x]
                      | (*uniq-table*[x]
                                       := make(<table>, test: \==, size: 10));
  //   At this point, car-table is a hash-table that either has
  //   (cons x y) in it, hashed under the key y, or we create 
  //   such an item and store it.
  car-table[y] | (car-table[y] := pair(x, y));
end method ucons;

define method umapcar (f, x)
  if (empty?(x)) #f; else ucons(f(head(x)), umapcar(f, tail(x))); end if;
end method umapcar;

// LTD: No macros.
#"ulist";

define method uappend (r, s)
  if (empty?(r)) s; else ucons(head(r), uappend(tail(r), s)); end if;
end method uappend;

