rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/misc.dylan,v 1.3 2001/03/14 23:34:30 bruce Exp $
copyright: see below
module: dylan-viscera


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// assert -- exported from Extensions.  Users of assert() should not
// have side-effects in the expression that is passed to assert(),
// because if we ever turn assertions off, that would mean the program
// runs differently in debug mode than it does in release mode.
//
define function assert (value) => ();
  unless (value)
    error("Assertion failed.");
  end;
end function assert;


// <not-supplied-marker> -- internal.
//
// The class of $not-supplied.
// 
define class <not-supplied-marker> (<object>)
end;

// $not-supplied -- exported from Extensions.
//
// a magic marker used to flag unsupplied keywords.
// 
define constant $not-supplied :: <not-supplied-marker>
    = make(<not-supplied-marker>);


// <never-returns> -- exported from Extensions.
//
// The empty type.  When used as a function result type, it means the function
// never returns.
//
define constant <never-returns> :: <type> = type-union();

define flushable generic values-sequence (sequence :: <sequence>);

define inline method values-sequence (sequence :: <sequence>)
  let vec :: <simple-object-vector> = as(<simple-object-vector>, sequence);
  values-sequence(vec);
end;

define inline method values-sequence
    (vector :: <simple-object-vector>)
  %%primitive(values-sequence, vector);
end;


define movable generic values (#rest values);

define inline method values (#rest values)
  %%primitive(values-sequence, values);
end;


define inline method object-address (object :: <object>)
    => res :: <raw-pointer>;
  %%primitive(object-address, object);
end method object-address;


define inline method ignore (#rest noise) => ();
end method ignore;


%%primitive(magic-internal-primitives-placeholder);
