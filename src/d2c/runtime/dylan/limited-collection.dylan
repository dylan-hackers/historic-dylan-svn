RCS-Header: $Header: /scm/cvs/src/d2c/runtime/dylan/limited-collection.dylan,v 1.1.2.6 2000/06/25 21:11:33 emk Exp $
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
//  and add your implementation class to limited-collection-implementation.
//  You'll also need to export your class from bootstrap.dylan and the
//  'magic' module of exports.dylan so you can refer to it from inside
//  the compiler.
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
//  All limited collection types[*] should inherit from this class. We use
//  it to tie into type introspection and cheap IO.
//  [*] Except the <simple-object-something> types, which aren't really
//  limited in any way.
//
//  XXX - We should be called <limited-collection>, but that name is
//  already taken by a subclass of <limited-type>. Investigate doing this
//  renaming someday--it will require changes to cback and possibly other
//  parts of the compiler.

define class <limited-collection-mixin> (<collection>)
  // What limited type was used to create this collection?
  slot %limited-collection-type :: <limited-collection>,
    required-init-keyword: collection-type:;
end class;

define sealed domain initialize (<limited-collection-mixin>);

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

define sealed domain make (singleton(<limited-object-table>));

define method make-limited-collection
    (base-class :: type-union(singleton(<table>), singleton(<object-table>)),
     element-type == <object>,
     collection-type :: <limited-collection>,
     #rest supplied-keys, #key, #all-keys)
 => (instance :: <simple-object-table>)
  // The DRM discussion of '<simple-object-vector>' and
  // 'limited(<simple-vector>, of: <object>)' on page 223
  // implies that we should do this exactly so.
  apply(make, <simple-object-table>, supplied-keys);
end method make-limited-collection;

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
//  %limited-simple-vector-class
//=========================================================================
//  This is a slightly more intelligent version of limited-vector-class.
//  It defines types which are properly hooked into the runtime type model.
//  XXX - Not complete yet.

define macro %limited-simple-vector-class
  { internal-limited-simple-vector-class(?:name,
					 ?element-type:expression,
					 ?fill:expression) }
    => { begin
	   define sealed class ?name (<simple-vector>,
				      <limited-collection-mixin>)
	     sealed slot %elem :: ?element-type,
	       init-value: ?fill, init-keyword: fill:, sizer: size,
	       size-init-value: 0, size-init-keyword: size:;
	   end class;
           define sealed domain make (singleton(?name));
	   define sealed inline method element-type
	       (class :: subclass(?name))
	    => (type :: <type>, indefinite? :: <false>);
	     values(?element-type, #f);
	   end method element-type;
           define sealed inline method element
	       (vec :: ?name, index :: <integer>,
		#key default = $not-supplied)
	    => element :: <object>; // because of default:
	     if (index >= 0 & index < vec.size)
	       %elem(vec, index);
	     elseif (default == $not-supplied)
	       element-error(vec, index);
	     else
	       default;
	     end;
	   end;
           define sealed inline method element-setter
	       (new-value :: ?element-type, vec :: ?name,
		index :: <integer>)
	    => new-value :: ?element-type;
	     if (index >= 0 & index < vec.size)
	       %elem(vec, index) := new-value;
	     else
	       element-error(vec, index);
	     end;
	   end;
           // This method is identical to the one in "array.dylan", except
           // that it is more tightly specialized to a single sealed class.
           // If you need to make a general change, you should probably grep
           // for "outlined-iterator" and change all matching locations.
           //
           define sealed inline method forward-iteration-protocol (array :: ?name)
	    => (initial-state :: <integer>,
		limit :: <integer>,
		next-state :: <function>,
		finished-state? :: <function>,
		current-key :: <function>,
		current-element :: <function>,
		current-element-setter :: <function>,
		copy-state :: <function>);
	     values(0,
		    array.size,
		    method (array :: ?name, state :: <integer>)
		     => new-state :: <integer>;
		      state + 1;
		    end,
		    method (array :: ?name, state :: <integer>,
			    limit :: <integer>)
		     => done? :: <boolean>;
		      // We use >= instead of == so that the constraint propagation
		      // stuff can tell that state is < limit if this returns #f.
		      state >= limit;
		    end,
		    method (array :: ?name, state :: <integer>)
		     => key :: <integer>;
		      state;
		    end,
		    method (array :: ?name, state :: <integer>)
		     => element :: ?element-type;
		      element(array, state);
		    end,
		    method (new-value :: ?element-type, array :: ?name,
			    state :: <integer>)
		     => new-value :: ?element-type;
		      element(array, state) := new-value;
		    end,
		    method (array :: ?name, state :: <integer>)
		     => state-copy :: <integer>;
		      state;
		    end);
	   end;
         end; }
end macro;


//=========================================================================
//  <simple-integer-vector>
//=========================================================================
//  We use a handy macro to declare our limited vector class. It, in turn,
//  uses funky compiler magic. ;-)

%limited-simple-vector-class(<simple-integer-vector>, <integer>, 0);

define method make-limited-collection
    (base-class :: type-union(singleton(<vector>), singleton(<simple-vector>)),
     element-type == <integer>,
     collection-type :: <limited-collection>,
     #rest supplied-keys,
     #key fill, size = $not-supplied, #all-keys)
 => (instance :: <simple-integer-vector>)

  // Bruce wants us to default 'size:' in the useful fashion.
  let restricted-size = collection-type.limited-size-restriction;
  let requested-size :: <integer> =
    case
      size ~= $not-supplied =>
	size;
      restricted-size =>
	restricted-size;
      otherwise =>
	0;
    end case;

  // XXX - These may not be the right errors to signal. Oh, well.
  if (restricted-size & requested-size ~= restricted-size)
    error("Requested vector size %= does not match size of %=",
	  size, collection-type);
  end if;
  if (requested-size > 0 & ~instance?(fill, <integer>))
    error("Cannot fill %= with %=", collection-type, fill);
  end if;

  apply(make, <simple-integer-vector>,
	collection-type: collection-type,
	size: requested-size, fill: fill,
	supplied-keys);
end method make-limited-collection;
