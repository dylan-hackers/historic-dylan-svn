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

// Strings
//
// Seals for most collection operations on the built-in collections can be
// found in seals.dylan.  Some exceptions apply, such as "make" and "as".
// See seals.dylan for more info.
//

// General string stuff.

define open abstract class <string> (<mutable-sequence>)
end;

define sealed inline method make (class == <string>, #key size = 0, fill = ' ')
    => res :: <byte-string>;
  make(<byte-string>, size: size, fill: fill);
end;

define sealed inline method as (class == <string>, collection :: <collection>)
    => res :: <byte-string>;
  as(<byte-string>, collection);
end;

define inline method as (class == <string>, string :: <string>)
    => res :: <string>;
  string;
end;

define method \< (str1 :: <string>, str2 :: <string>) => res :: <boolean>;
  block (return)
    for (char1 in str1, char2 in str2)
      if (char1 < char2)
        return(#t);
      elseif (char2 < char1)
        return(#f);
      end;
    end;
    str1.size < str2.size;
  end;
end;

define method \< (str1 :: <byte-string>, str2 :: <byte-string>) => res :: <boolean>;
  block (return)
    for (char1 in str1, char2 in str2)
      if (char1 < char2)
        return(#t);
      elseif (char2 < char1)
        return(#f);
      end;
    end;
    str1.size < str2.size;
  end;
end;

define method as-lowercase (str :: <string>)
    => res :: <string>;
  map(as-lowercase, str);
end;

define method as-lowercase! (str :: <string>)
    => res :: <string>;
  map-into(str, as-lowercase, str);
end;

define method as-uppercase (str :: <string>)
    => res :: <string>;
  map(as-uppercase, str);
end;

define method as-uppercase! (str :: <string>)
    => res :: <string>;
  map-into(str, as-uppercase, str);
end;

define method as-lowercase (string :: <byte-string>)
    => result :: <byte-string>;
  as-lowercase!(shallow-copy(string));
end;

define inline method as-lowercase! (string :: <byte-string>)
    => result :: <byte-string>;
  call-out("gd_byte_string_as_lowercase", void:,
           ptr: vector-elements-address(string),
           int: string.size);
  string;
end;

define method as-uppercase (string :: <byte-string>)
    => result :: <byte-string>;
  as-uppercase!(shallow-copy(string));
end;

define inline method as-uppercase! (string :: <byte-string>)
    => result :: <byte-string>;
  call-out("gd_byte_string_as_uppercase", void:,
           ptr: vector-elements-address(string),
           int: string.size);
  string;
end;


// Built-in strings.


// Unicode strings.

define class <unicode-string> (<string>, <vector>)
  sealed slot %element :: <character>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end;

define sealed domain make (singleton(<unicode-string>));

define sealed method as (class == <unicode-string>, collection :: <collection>)
    => res :: <unicode-string>;
  let res = make(<unicode-string>, size: collection.size);
  for (index :: <integer> from 0, element in collection)
    res[index] := element;
  end;
  res;
end;

define inline method as (class == <unicode-string>, string :: <unicode-string>)
    => res :: <unicode-string>;
  string;
end;

define inline method element
    (vec :: <unicode-string>, index :: <integer>,
     #key default = $not-supplied)
    => element :: <object>; // because of default:
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define inline method element-setter
    (new-value :: <character>, vec :: <unicode-string>,
     index :: <integer>)
    => new-value :: <character>;
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
define inline method forward-iteration-protocol (array :: <unicode-string>)
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
	 method (array :: <unicode-string>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <unicode-string>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   // We use >= instead of == so that the constraint propagation
	   // stuff can tell that state is < limit if this returns #f.
	   state >= limit;
	 end,
	 method (array :: <unicode-string>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <unicode-string>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <unicode-string>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <unicode-string>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;


// Byte strings.

define class <byte-string> (<string>, <vector>)
  sealed slot %element :: <byte-character>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end;

define sealed domain make (singleton(<byte-string>));

define inline method element
    (vec :: <byte-string>, index :: <integer>,
     #key default = $not-supplied)
    => element :: <object>; // because of default:
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define inline method element-setter
    (new-value :: <byte-character>, vec :: <byte-string>,
     index :: <integer>)
    => new-value :: <byte-character>;
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
define inline method forward-iteration-protocol (array :: <byte-string>)
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
	 method (array :: <byte-string>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <byte-string>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   // We use >= instead of == so that the constraint propagation
	   // stuff can tell that state is < limit if this returns #f.
	   state >= limit;
	 end,
	 method (array :: <byte-string>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <byte-string>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <byte-string>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <byte-string>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;

// as type coercion methods
//

define sealed method as
    (class == <byte-string>, collection :: <collection>)
    => res :: <byte-string>;
  // We won't blindly trust that size(collection) returns a 
  // value consistent with the forward-iteration-protocol for 
  // the collection so let's be sure not to iterate past sz.
  // (It's possible that a user-implemented collection class 
  // could be incorrect.)
  // Also don't use the keyed-by clause here because it may be
  // slow for the collection.
  let sz :: false-or(<integer>) = collection.size;
  if (~sz)
    unbounded-collection-error(as, collection);
  else
    let res = make(<byte-string>, size: sz);
    for (index from 0 below sz, elt :: <byte-character> in collection)
      %element(res, index) := elt;
    end;
    res;
  end if;
end method as;

define sealed method as
    (class == <byte-string>, list :: <list>)
    => res :: <byte-string>;
  let sz :: false-or(<integer>) = list.size;
  if (~sz)
    unbounded-collection-error(as, list);
  else
    let res = make(<byte-string>, size: sz);
    for (index from 0, elt :: <byte-character> in list)
      %element(res, index) := elt;
    end;
    res;
  end if;
end method as;

define sealed method as
    (class == <byte-string>, vec :: <simple-object-vector>)
    => res :: <byte-string>;
  let res = make(<byte-string>, size: vec.size);
  for (elt :: <byte-character> keyed-by index in vec)
    %element(res, index) := elt;
  end;
  res;
end method as;

define sealed inline method as
    (class == <byte-string>, string :: <byte-string>)
    => res :: <byte-string>;
  string;
end method as;

define sealed method as
    (class == <byte-string>, ssv :: <stretchy-object-vector>)
    => res :: <byte-string>;
  let sz = ssv.size;
  let res = make(<byte-string>, size: sz);
  let data = ssv.ssv-data;
  for (index from 0 below sz)
    %element(res, index) := check-type(%element(data, index), <byte-character>);
  end;
  res;
end method as;

// author: PDH
define sealed method as
    (class == <byte-string>, deq :: <object-deque>)
    => res :: <byte-string>;
  let res = make(<byte-string>, size: deq.size);
  for (elt :: <byte-character> keyed-by index in deq)
    %element(res, index) := elt;
  end;
  res;
end method as;


// Not strictly necessary, but produces slightly more optimal code
//
define inline method type-for-copy (object :: <byte-string>)
 => (class :: <class>)
  <byte-string>;
end;

// author: PDH
define method fill!
    (string :: <byte-string>, value :: <byte-character>,
     #key start :: <integer> = 0, end: last :: false-or(<integer>))
 => (string :: <byte-string>);
  let last = check-start-end-bounds(fill!, string, start, last);
  for (index from start below last)
    %element(string, index) := value;
  end;
  string;
end method;

// author: PDH
// This is essentially a specialized clone of the general method for sequences
define method remove
    (string :: <byte-string>, value :: <byte-character>,
     #key test :: <function> = \==, count :: false-or(<integer>))
 => (result :: <byte-string>);
  for (elem in string,
       result = #() then if ((count & (count <= 0))
                             | ~compare-using-default-==(test, elem, value))
                            pair(elem, result);
                         else
                           if (count) count := count - 1 end;
                           result;
                         end if)
  finally
    as(<byte-string>, reverse!(result));
  end for;
end method remove;

// author: PDH, 5x speed-up
define method reverse (string :: <byte-string>)
 => (result :: <byte-string>)
  let sz = string.size;
  let result = make(<byte-string>, size: sz);
  for (elt in string, reverse-index from (sz - 1) by -1)
    %element(result, reverse-index) := elt;
  end;
  result;
end method;

// author: PDH, 50x speed-up
define method reverse! (string :: <byte-string>)
 => (result :: <byte-string>)
  let sz = string.size;
  let mid = ash(sz, -1);
  for (left from 0 below mid, right from (sz - 1) by -1)
    %swap-elements!(string, left, right);
  end;
  string;
end method;

// author: PDH, 4x speed-up
define method copy-sequence
    (source :: <byte-string>, #key start :: <integer> = 0, end: last :: false-or(<integer>))
 => (result :: <byte-string>);
  let last = check-start-end-bounds(copy-sequence, source, start, last);
  let dest-size = last - start;
  let dest = make(<byte-string>, size: dest-size);
  // use an empirically determined cut-off point to switch to memcpy
  if (dest-size < $memcpy-switchover-point)
    for (dest-index from 0 below dest-size, source-index from start)
      %element(dest, dest-index) := %element(source, source-index);
    end;
  else
    call-out("memcpy", void:,
       ptr: vector-elements-address(dest),
       ptr: vector-elements-address(source) + start,
       int: dest-size);  
  end if;
  dest;
end method;

// author: PDH, 5x speed-up
define inline method shallow-copy (string :: <byte-string>)
 => (result :: <byte-string>)
  let sz :: <integer> = string.size;
  let result = make(<byte-string>, size: sz);
  // use an empirically determined cut-off point to switch to memcpy
  if (sz < $memcpy-switchover-point)
    for (elt keyed-by index in string)
      %element(result, index) := elt;
    end;
  else
    call-out("memcpy", void:,
       ptr: vector-elements-address(result),
       ptr: vector-elements-address(string),
       int: sz);
  end if;
  result;
end method;

// author: PDH, 1.5x speed-up
// For average-length strings, calling out to the C library function
// memcmp is slower than comparing entirely in Dylan.
//
define method \= (str1 :: <byte-string>, str2 :: <byte-string>)
 => (res :: <boolean>);
  if (str1 == str2)
    #t;
  elseif (str1.size ~== str2.size)
    #f;
  else
    // The strings are equal in size.
    block (return)
      // Do a character by character compare.
      for (char1 keyed-by index in str1)
        if (char1 ~== %element(str2, index))
          return(#f);
        end if;
      end for;
      #t;
    end block;
  end if;
end;

define method concatenate-as (type == <byte-string>, sequence :: <byte-string>,
                              #rest more-sequences)
 => result :: <byte-string>;
  let needed-size :: <integer> = sequence.size;

  local method applicable? () => answer :: <boolean>;
    block (break)
      for (seq in more-sequences)
        if (instance?(seq, <byte-string>))
          needed-size := needed-size + seq.size;
        else
          break(#f);
        end;
      end for;
      #t;
    end block;
  end;

  if (~applicable?())
    next-method();
  else
    let result = make(<byte-string>, size: needed-size);
    let sequence-size = sequence.size;
    copy-bytes(result, 0, sequence, 0, sequence-size);
    let result-position = sequence-size;
    for (seq :: <byte-string> in more-sequences)
      let seq-size = seq.size;
      copy-bytes(result, result-position, seq, 0, seq-size);
      result-position := result-position + seq-size;
    end;
    result;
  end if;
end method concatenate-as;

/*
  The concatenate method below can be deleted when 
  the following bug affecting "apply" is fixed:
  https://gauss.gwydiondylan.org/bugs/show_bug.cgi?id=7146
*/
define method concatenate (sequence :: <byte-string>,
                           #rest more-sequences)
 => result :: <byte-string>;
  let needed-size :: <integer> = sequence.size;

  local method applicable? () => answer :: <boolean>;
    block (break)
      for (seq in more-sequences)
        if (instance?(seq, <byte-string>))
          needed-size := needed-size + seq.size;
        else
          break(#f);
        end;
      end for;
      #t;
    end block;
  end;

  if (~applicable?())
    next-method();
  else
    let result = make(<byte-string>, size: needed-size);
    let sequence-size = sequence.size;
    copy-bytes(result, 0, sequence, 0, sequence-size);
    let result-position = sequence-size;
    for (seq :: <byte-string> in more-sequences)
      let seq-size = seq.size;
      copy-bytes(result, result-position, seq, 0, seq-size);
      result-position := result-position + seq-size;
    end;
    result;
  end if;
end method concatenate;
