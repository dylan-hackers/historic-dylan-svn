module: opengl
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

/*********
 *  This file contains macros used by opengl.intr and others.
 *  (this isn't in opengl.intr, as melange hates macros and me)
 *  
 *  Thanks to Eric Kidd for these macros.
 *********/

/* The function-family-[1234] macros generate a generic function and a
   method for each clause.  OpenGL uses different function names for
   essentially identical functions with different argument types.  In
   Dylan, we want them all to have one name (glColor instead of
   glColor3f)

INPUT:
   define function-family-2 glVertex2
     <integer>      => glVertex2i;
     <single-float> => glVertex2f;
     <double-float> => glVertex2d;
   end function-family-2;

EXPANSION:
   define sealed generic glVertex2(x :: <number>, y :: <number>) => ();

   define inline method glVertex2
       (x :: <double-float>, y :: <double-float>) => ()
     glVertex2d(x, y);
   end method;

   define inline method glVertex2
       (x :: <single-float>, y :: <single-float>) => ()
     glVertex2f(x, y);
   end method;

   define inline method glVertex2(x :: <integer>, y :: <integer>) => ()
     glVertex2i(x, y);
   end method;
*/

define macro function-family-1-definer
  { define function-family-1 ?:name ?clauses:* end }
    => { define sealed generic ?name 
             (a :: <number>) => ();
         function-family-1-aux ?name ?clauses end }
end macro;


define macro function-family-1-aux
  { function-family-1-aux ?:name ?class:name => ?func:name; ?stuff:* end }
    => { define inline method ?name
             (a :: ?class) => ()
	   ?func(a);
         end method;
         function-family-1-aux ?name ?stuff end }
  { function-family-1-aux ?:name end } => { }
end macro;

// 2 arguments:
define macro function-family-2-definer
  { define function-family-2 ?:name ?clauses:* end }
    => { define sealed generic ?name 
             (a :: <number>, b :: <number>) => ();
         function-family-2-aux ?name ?clauses end }
end macro;


define macro function-family-2-aux
  { function-family-2-aux ?:name ?class:name => ?func:name; ?stuff:* end }
    => { define inline method ?name
             (a :: ?class, b :: ?class) => ()
	   ?func(a, b);
         end method;
         function-family-2-aux ?name ?stuff end }
  { function-family-2-aux ?:name end } => { }
end macro;


// Now for 3 arguments...
define macro function-family-3-definer
  { define function-family-3 ?:name ?clauses:* end }
    => { define sealed generic ?name 
             (a :: <number>, b :: <number>, c :: <number>) => ();
         function-family-3-aux ?name ?clauses end }
end macro;


define macro function-family-3-aux
  { function-family-3-aux ?:name ?class:name => ?func:name; ?stuff:* end }
    => { define inline method ?name
             (a :: ?class, b :: ?class, c :: ?class) => ()
	   ?func(a, b, c);
         end method;
         function-family-3-aux ?name ?stuff end }
  { function-family-3-aux ?:name end } => { }
end macro;

// Now for 4 arguments...
define macro function-family-4-definer
  { define function-family-4 ?:name ?clauses:* end }
    => { define sealed generic ?name 
             (a :: <number>, b :: <number>, c :: <number>, d :: <number>) 
          => ();
         function-family-4-aux ?name ?clauses end }
end macro;


define macro function-family-4-aux
  { function-family-4-aux ?:name ?class:name => ?func:name; ?stuff:* end }
    => { define inline method ?name
             (a :: ?class, b :: ?class, c :: ?class, d :: ?class) => ()
	   ?func(a, b, c, d);
         end method;
         function-family-4-aux ?name ?stuff end }
  { function-family-4-aux ?:name end } => { }
end macro;

