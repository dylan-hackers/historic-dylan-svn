//  RAMER.CL
//  A Common Lisp implementation of Ramer's recursive algorithm
//  for approximating a polygon with another polygon.
//  (C) Copyright 1995 by Steven L. Tanimoto.
//  This program is described in Chapter 12 ("Vision") of
//  "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
//  published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
//  Permission is granted for noncommercial use and modification of
//  this program, provided that this copyright notice is retained
//  and followed by a notice of any modifications made to the program.
//  Here is a test polygon with 8 vertices (7 sides).
define variable *test-polygon* =
  #(#(0.0, 0.0), #(0.0, 4.0), #(2.0, 6.0), #(4.0, 6.0), #(6.0, 4.0),
    #(6.0, 0.0), #(4.0, -2.0), #(2.0, -2.0));

//  declare global variable:
define variable *tolerance* = 5.0;

//  RAMER is the top-level function.
define method ramer (poly)
  // Returns the entire polygonal approximation of POLY.
  concatenate(ramer1(poly), copy-sequence(poly, start: size(poly) - 1));
end method ramer;

//  RAMER1 is the recursive function which computes the
//  current error of approximation and if exceeded, splits
//  POLY and calls itself recursively on the two pieces.
define method ramer1 (poly)
  // Returns polygonal approx. of poly, without last pt.
  let error-list = errors(poly);
  let max-error = apply(max, error-list);
  let two-halves = #f;
  if (*tolerance* > max-error)
    list(first(poly));
    //  approx. ok, return 1st point.
    else
    two-halves := split(poly, error-list, max-error);
    concatenate(ramer1(first(two-halves)), ramer1(second(two-halves)));
  end if;
end method ramer1;

define method split (poly, error-list, max-error)
  // Returns a list of two lists, obtained by breaking POLY
  //    at the vertex corresponding to the value MAX-ERROR
  //    on ERROR-LIST.
  if (first(error-list) = max-error)
    list(list(first(poly)), poly);
  else
    let temp = split(tail(poly), tail(error-list), max-error);
    pair(pair(first(poly), first(temp)), tail(temp));
  end if;
end method split;

define method errors (poly)
  // Returns a list of the approximation errors at each
  //    of the internal points of POLY.
  let lastpoint = first(copy-sequence(poly, start: size(poly) - 1));
  let x1 = head(head(poly));
  let y1 = cadar(poly);
  let x2 = first(lastpoint);
  let y2 = second(lastpoint);
  pair(0.0, //  error for first point is clearly 0.
       map(method (p) dist3(first(p), second(p), x1, y1, x2, y2); end method,
           tail(poly)));
end method errors;

define method dist3 (x0, y0, x1, y1, x2, y2)
  // Returns the square of the minimum distance between
  //    the point (X0 Y0) and the line segment whose endpoints
  //    are (X1 Y1) and (X2 Y2).
  min(distsq(x0, y0, x1, y1), distsq(x0, y0, x2, y2),
      pdist(x0, y0, x1, y1, x2, y2));
end method dist3;

define method pdist (x0, y0, x1, y1, x2, y2)
  // Computes and returns the square of the perpendicular distance
  //    from the point (X0 Y0) to the line that passes through
  //    points (X1 Y1) and (X2 Y2).
  let a = x1 - x2;
  let b = y1 - y2;
  let c = x0 - x1;
  let d = y0 - y1;
  let temp = a * d - b * c;
  temp * temp / (a * a + b * b);
end method pdist;

define method distsq (x1, y1, x2, y2)
  // Computes and returns the square of the Euclidean distance
  //    between the two points (X1 Y1) and (X2 Y2).
  let a = x1 - x2;
  let b = y1 - y2;
  a * a + b * b;
end method distsq;

//  Here is a simple test routine.
define method test ()
  // Calls RAMER on *TEST-POLYGON*.
  format-out("\nApproximation of test polygon:\n %=", ramer(*test-polygon*));
end method test;

// LTD: Function TRACE not yet implemented.
trace(ramer, split);

test();

//  test data for the exercise in the text:
//  (defparameter *sine*
//    '((0.0 0.0)(1.0 2.0)(3.0 -2.0)(5.0 2.0)
//      (7.0 -2.0)(9.0 2.0)(10.0 0.0) ) )
//  (defparameter *cosine*
//    '((0.0 2.0)(2.0 -2.0)(4.0 2.0)(6.0 -2.0)
//      (8.0 2.0)(10.0 -2.0) ) )
"eof";

