RCS-Header: $Header: /scm/cvs/src/d2c/runtime/dylan/limited-collection.dylan,v 1.1.2.3 2000/06/25 06:16:18 emk Exp $
Module: dylan-viscera
Copyright: See below.
Synopsis: Runtime support for limited collections.
Author: Eric Kidd <eric.kidd@pobox.com>

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

// XXX - We should rename this slot on <limited-type>, but we can't do that
// without modifying cback's heap-dumping code and incrementing the
// bootstrap counter.
define constant limited-type-base-class = limited-integer-base-class;


//=========================================================================
//  make(singleton(<limited-collection>))
//=========================================================================
//  We pick apart the limited collection type and do (another) dispatch.
//
//  To add a new limited collection type, define an appropriate method on
//  make-limited-collection. Then, go to compiler/optimize/limopt.dylan
//  and add your implementation class to limited-collection-cclass.
//
//  This function should *not* be inlined. We define a transformer that
//  shadows this function and generates better (but equivalent) code.
//  If you inline this function, the transformer breaks.

define sealed  method make
    (type :: <limited-collection>, #rest supplied-keys, #key, #all-keys)
 => (instance :: <collection>)
  apply(make-limited-collection,
	type.limited-type-base-class,
	type.limited-element-type,
	type, supplied-keys);
end method make;

define generic make-limited-collection
    (base-class :: <class>,
     element-type :: <type>,
     collection-type :: <limited-collection>,
     #key, #all-keys)
 => (instance :: <collection>);


//=========================================================================
//  <limited-collection-mixin>
//=========================================================================
// Still thinking about this...
//
// XXX - We should be called <limited-collection>, but that name is already
// taken by a subclass of <limited-type>. Investiage doing this renaming
// someday--it will require changes to cback and possibly other parts of
// the compiler.

define class <limited-collection-mixin> (<collection>)
  // What limited type was used to create this collection?
  slot %limited-collection-type :: <limited-collection>,
    required-init-keyword: collection-type:;
end class;

define method element-type (collection :: <limited-collection-mixin>)
 => (type :: <type>, indefinite? :: <boolean>);
  element-type(collection.%limited-collection-type);
end method element-type;


//=========================================================================
//  <limited-object-table>
//=========================================================================
//  We just override element-setter.

define class <limited-object-table>
    (<object-table>, <limited-collection-mixin>)
end class <limited-object-table>;

define method make-limited-collection
    (base-class :: type-union(singleton(<table>), singleton(<object-table>)),
     element-type :: <type>,
     collection-type :: <limited-collection>,
     #rest supplied-keys, #key, #all-keys)
 => (instance :: <limited-object-table>)
  apply(make, <limited-object-table>,
	collection-type: collection-type,
	supplied-keys);
end method make-limited-collection;

define method element-setter
    (new-value :: <object>, collection :: <limited-object-table>,
     key :: <object>)
 => (element :: <object>)
  check-type(new-value, collection.%limited-collection-type.element-type);
  next-method();
end method element-setter;


//=========================================================================
//  <simple-integer-vector>
//=========================================================================
//  We use a handy macro from vector.dylan to define our limited vector
//  class. It, in turn, uses funky compiler magic. ;-)

define limited-collection <simple-integer-vector> (<vector>) of <integer> = 0;

define method make-limited-collection
    (base-class :: type-union(singleton(<vector>), singleton(<simple-vector>)),
     element-type == <integer>,
     collection-type :: <limited-collection>,
     #rest supplied-keys,
     #key fill, size = 0, #all-keys)
 => (instance :: <simple-integer-vector>)
  let size-restriction = collection-type.limited-size-restriction;
  // XXX - These may not be the right errors to signal. Oh, well.
  if (size-restriction & size ~= size-restriction)
    error("Requested vector size does not match size of %s", collection-type);
  end if;
  if (size > 0 & ~instance?(fill, <integer>))
    error("Cannot fill %s with %=", collection-type, fill);
  end if;
  apply(make, <simple-integer-vector>,
	collection-type: collection-type,
	size: size, fill: fill,
	supplied-keys);
end method make-limited-collection;
