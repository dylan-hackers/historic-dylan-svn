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

// Vectors
//
// Seals for most collection operations on the built-in collections can be
// found in seals.dylan.  Some exceptions apply, such as "make" and "as".
// See seals.dylan for more info.
//

// Abstract vector stuff.

define open abstract class <vector> (<array>)
end;

define sealed inline method make (class == <vector>, #key size = 0, fill)
    => res :: <simple-object-vector>;
  make(<simple-object-vector>, size: size, fill: fill);
end;

define sealed inline method as
    (class == <vector>, collection :: <collection>)
    => res :: <vector>;
  as(<simple-object-vector>, collection);
end;

define sealed inline method as
    (class == <vector>, vector :: <vector>)
    => res :: <vector>;
  vector;
end;


// out-of-line error functions to minimize calling code size

define not-inline function row-major-index-error (index :: <integer>)
 => res :: <never-returns>;
  error("Vector index out of bounds: %=", index);
end function;

define not-inline function vector-rank-error (indices)
 => res :: <never-returns>;
  error("Number of indices not equal to rank. Got %=, wanted one index",
	indices);
end function;



// In the DRM, the array methods dimension, dimensions, row-major-index, 
// are not specified as sealed for vectors, but they probably should be
// since it seems rather impossible that user could provide a different 
// implementation that is more efficient or produces different results.
//

define sealed inline method rank (vec :: <vector>) => res :: <integer>;
  1;
end;

define sealed inline method dimension (vec :: <vector>, axis :: <integer>)
    => dimension :: <integer>;
  if (axis ~== 0)
    axis-error(vec, axis);
  else
    vec.size;
  end if;
end;

define sealed inline method dimensions (vec :: <vector>) => res :: <simple-object-vector>;
  vector(vec.size);
end;

/*
  Move the body of the internal method below back to 
  the orginal method and remove this method when 
  the following bug affecting "apply" is fixed:
  https://gauss.gwydiondylan.org/bugs/show_bug.cgi?id=7146
*/
define inline method row-major-index-internal (vec :: <vector>, indices :: <rest-parameters-sequence>)
    => index :: <integer>;
  if (indices.size ~== 1)
    vector-rank-error(indices);
  end if;
  let index :: <integer> = %element(indices, 0);
  if (index < 0 | index >= vec.size)
    row-major-index-error(index);
  end;
  index;
end;

define sealed inline method row-major-index (vec :: <vector>, #rest indices)
    => index :: <integer>;
  row-major-index-internal(vec, indices);
end;


// <simple-vector>s

define sealed abstract class <simple-vector> (<vector>)
end;

define sealed inline method make
    (class == <simple-vector>, #key size = 0, fill)
    => res :: <simple-object-vector>;
  make(<simple-object-vector>, size: size, fill: fill);
end;

define sealed inline method as
    (class == <simple-vector>, collection :: <collection>)
    => res :: <simple-vector>;
  as(<simple-object-vector>, collection);
end;

define sealed inline method as
    (class == <simple-vector>, vector :: <simple-vector>)
    => res :: <simple-vector>;
  vector;
end;

// TO DO: implement reverse, reverse!, copy-sequence, shallow-copy, 
// and most other methods for limited vectors.  They (and fill! below) 
// should probably be implemented in the limited vector macros, 
// specialized for each type, to get maximum speed.

// author: PDH
define method fill!
    (vec :: <simple-vector>, value :: <object>,
     #key start :: <integer> = 0, end: last :: false-or(<integer>))
 => (vec :: <simple-vector>);
  let (seq-size, last) = check-start-end-bounds("fill!", vec, start, last);
  for (index from start below last)
    element(vec, index) := value;
  end;
  vec;
end method;


// <simple-object-vector>s

define inline function vector (#rest args) => res :: <simple-object-vector>;
  args;
end;

define class <simple-object-vector> (<simple-vector>)
  sealed slot %element,
    init-value: #f, init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end;

define sealed domain make (singleton(<simple-object-vector>));

define sealed inline method element
    (vec :: <simple-object-vector>, index :: <integer>,
     #key default = $not-supplied)
    => elt :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: <object>, vec :: <simple-object-vector>,
     index :: <integer>)
    => new-value :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

// This method is identical to the one in "array.dylan", except that it
// is more tightly specialized to a single sealed class.  If you need to 
// make a general change, you should probably grep for "outlined-iterator" 
// and change all matching locations.
//
define sealed inline method forward-iteration-protocol
    (array :: <simple-object-vector>)
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
	 method (array :: <simple-object-vector>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <simple-object-vector>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   // We use >= instead of == so that the constraint propagation
	   // stuff can tell that state is < limit if this returns #f.
	   state >= limit;
	 end,
	 method (array :: <simple-object-vector>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <simple-object-vector>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <simple-object-vector>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <simple-object-vector>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;

define sealed inline method as
    (class == <simple-object-vector>, vec :: <simple-object-vector>)
    => res :: <simple-object-vector>;
  vec;
end;

// Perhaps this should be inlined eventually, but at present it
// tickles a bug in stack analysis
//
define sealed method as
    (class == <simple-object-vector>, collection :: <collection>)
    => res :: <simple-object-vector>;
  let sz :: <integer> = collection.size;
  let res = make(<simple-object-vector>, size: sz);
  // We won't blindly trust that size(collection) returns a 
  // value consistent with the forward-iteration-protocol for 
  // the collection so let's be sure not to iterate past sz.
  // (It's possible that a user-implemented collection class 
  // could be incorrect.)
  // Also don't used the key-by clause here because it may be
  // slow for collection.
  for (index :: <integer> from 0 below sz, elt in collection)
    %element(res, index) := elt;
  end;
  res;
end method as;

// This method looks to be unduly specific, but the compiler will
// generate this case whenever you "apply" a function to a list
//
define sealed inline method as
    (class == <simple-object-vector>, list :: <list>)
    => res :: <simple-object-vector>;
  let res = make(<simple-object-vector>, size: list.size);
  for (index :: <integer> from 0, elt in list)
    %element(res, index) := elt;
  end;
  res;
end method as;

// This method looks to be unduly specific, but the compiler will
// generate this case whenever you "apply" a function to a strechy vector
// Perhaps this should be inlined eventually, but at present it
// tickles a bug in stack analysis
//
define sealed method as
    (class == <simple-object-vector>, ssv :: <stretchy-object-vector>)
 => (res :: <simple-object-vector>);
  let sz = ssv.size;
  let res = make(<simple-object-vector>, size: sz);
  for (index :: <integer> from 0 below sz)
    %element(res, index) := %element(ssv, index);
  end;
  res;
end;

// Not strictly necessary, but produces slightly more optimal code
//
define inline method type-for-copy (object :: <simple-object-vector>)
 => (class :: <class>)
  <simple-object-vector>;
end;

// author: PDH, 1.2x speed increase
define method fill!
    (vec :: <simple-object-vector>, value :: <object>,
     #key start :: <integer> = 0, end: last :: false-or(<integer>))
 => (vec :: <simple-object-vector>);
  let (seq-size, last) = check-start-end-bounds("fill!", vec, start, last);
  for (index from start below last)
    %element(vec, index) := value;
  end;
  vec;
end method;

// author: PDH
// This is essentially a specialized clone of the general method for sequences
define method remove
    (vec :: <simple-object-vector>, value,
     #key test :: <function> = \==, count :: false-or(<integer>))
 => (result :: <simple-object-vector>);
  for (elem in vec,
       result = #() then if ((count & (count <= 0))
                             | ~compare-using-default-==(test, elem, value))
                            pair(elem, result);
                         else
                           if (count) count := count - 1 end;
                           result;
                         end if)
  finally
    as(<simple-object-vector>, reverse!(result));
  end for;
end method remove;

// author: PDH, 5x speed-up
define sealed method reverse (vec :: <simple-object-vector>)
 => (result :: <simple-object-vector>)
  let sz = vec.size;
  let result = make(<simple-object-vector>, size: sz);
  for (elt in vec, reverse-index from (sz - 1) by -1)
    %element(result, reverse-index) := elt;
  end;
  result;
end method;

// author: PDH, 50x speed-up
define sealed method reverse! (vec :: <simple-object-vector>)
 => (result :: <simple-object-vector>)
  let sz = vec.size;
  let mid = ash(sz, -1);
  for (left from 0 below mid, right from (sz - 1) by -1)
    %swap-elements!(vec, left, right);
  end;
  vec;
end method;

define constant $memcpy-switchover-point = 50;

// author: PDH, 4x speed-up
define method copy-sequence
    (src :: <simple-object-vector>, #key start :: <integer> = 0, end: last :: false-or(<integer>))
 => (result :: <simple-object-vector>);
  let (src-size, last) = check-start-end-bounds("copy-sequence", src, start, last);
  let dst-size = last - start;
  let dst = make(<simple-object-vector>, size: dst-size);
  // use an empirically determined cut-off point to switch to memcpy
  if (dst-size < $memcpy-switchover-point)
    for (dst-index from 0 below dst-size, src-index from start)
      %element(dst, dst-index) := %element(src, src-index);
    end;
  else
    call-out("memcpy", void:,
       ptr: vector-elements-address(dst),
       ptr: vector-elements-address(src) + start * c-expr(int: "sizeof(descriptor_t)"),
       int: dst-size * c-expr(int: "sizeof(descriptor_t)"));  
  end if;
  dst;
end method;

// author: PDH, 5x speed-up
define sealed inline method shallow-copy (vec :: <simple-object-vector>)
 => (result :: <simple-object-vector>)
  let sz :: <integer> = vec.size;
  let result = make(<simple-object-vector>, size: sz);
  // use an empirically determined cut-off point to switch to memcpy
  if (sz < $memcpy-switchover-point)
    for (elt keyed-by index in vec)
      %element(result, index) := elt;
    end;
  else
    call-out("memcpy", void:,
       ptr: vector-elements-address(result),
       ptr: vector-elements-address(vec),
       int: sz * c-expr(int: "sizeof(descriptor_t)"));
  end if;
  result;
end method;


define open generic %elem (vec :: <vector>, index :: <integer>) 
 => (result :: <object>);

define open generic %elem-setter
    (value :: <object>, vec :: <vector>, index :: <integer>) 
 => (result :: <object>);

define macro limited-vector-class
  { limited-vector-class(?:name, ?element-type:expression, ?fill:expression) }
    => { begin
	   define sealed class ?name (<vector>)
	     sealed slot %elem :: ?element-type,
	       init-value: ?fill, init-keyword: fill:, sizer: size,
	       size-init-value: 0, size-init-keyword: size:;
	   end class;
           define sealed domain make (singleton(?name));
           define sealed domain initialize (?name);
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

