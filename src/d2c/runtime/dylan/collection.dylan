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

// Classes

define abstract open class <collection> (<object>)
end;

define abstract open class <explicit-key-collection> (<collection>)
end;

define abstract open class <stretchy-collection> (<collection>)
end;

define abstract open class <mutable-collection> (<collection>)
end;

define abstract open class <sequence> (<collection>)
end;

define abstract open class <stretchy-sequence>
    (<stretchy-collection>, <sequence>)
end;

define abstract open class <mutable-explicit-key-collection>
    (<mutable-collection>, <explicit-key-collection>)
end;

define abstract open class <mutable-sequence>
    (<mutable-collection>, <sequence>)
end;


// Collection Generics.

define open generic forward-iteration-protocol
    (collection :: <collection>)
    => (initial-state :: <object>,
	limit :: <object>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);

define open generic backward-iteration-protocol
    (collection :: <collection>)
    => (initial-state :: <object>,
	limit :: <object>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);

// bounds-checking version
define open generic element
    (collection :: <collection>, key :: <object>, #key default)
 => (element :: <object>);

define open generic element-setter
    (new-value :: <object>, collection :: <mutable-collection>,
     key :: <object>)
 => (element :: <object>);

// non-checking version
// NB %element doesn't accept defaults because for vectors/strings etc
// it doesn't do bounds checking so the default is useless.
// A default *might* be wanted for e.g. hash tables, but it's not
// available, sorry.  Be more choosy where you put without-bounds-checks!!

define open generic %element
    (collection :: <collection>, key :: <object>)
 => (element :: <object>);

define open generic %element-setter
    (new-value :: <object>, collection :: <mutable-collection>,
     key :: <object>)
 => (element :: <object>);

define open generic key-sequence
    (collection :: <collection>) => (keys :: <sequence>);

define open generic size (object :: <object>) => (res :: false-or(<integer>));

define open generic empty?
    (collection :: <collection>)
    => res :: <boolean>;

define sealed generic do
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => false :: <false>;

define sealed generic map
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => new-collection :: <collection>;

define sealed generic map-as
    (type :: <type>, proc :: <function>, collection :: <collection>,
     #rest more-collections)
    => new-collection :: <mutable-collection>;

define sealed generic map-into
    (target :: <mutable-collection>, proc :: <function>,
     collection :: <collection>, #rest more-collections)
    => new-collection :: <mutable-collection>;

define sealed generic any?
    (pred :: <function>, collection :: <collection>, #rest more-collections)
    => value;

define sealed generic every?
    (pred :: <function>, collection :: <collection>, #rest more-collections)
    => boolean :: <boolean>;

define open generic reduce
    (proc :: <function>, initial-value :: <object>, collection :: <collection>)
    => value :: <object>;

define open generic reduce1
    (proc :: <function>, collection :: <collection>)
    => value :: <object>;

define open generic member?
    (value :: <object>, collection :: <collection>, #key test)
    => res :: <boolean>;

define open generic find-key
    (collection :: <collection>, pred :: <function>, #key skip, failure)
    => key :: <object>;

define open generic replace-elements!
    (target :: <mutable-collection>, pred :: <function>,
     new-value-fn :: <function>, #key count)
    => target :: <mutable-collection>;

define open generic fill!
    (target :: <mutable-collection>, value :: <object>, #key)
    => target :: <mutable-collection>;

define open generic key-test
    (collection :: <collection>) => (test-function :: <function>);


// This type-for-copy method is specified in the DRM, but I'm not
// convinced it does anything useful since the method for <object>
// does the same thing.  This issue in the DRM has been flagged
// for further review.
//
define inline method type-for-copy (collection :: <mutable-collection>)
    => class :: <class>;
  collection.object-class;
end method type-for-copy;

// Collection Methods.

// default methods for %element and %element-setter just call through
// to the standard element and element-setter.  This is for the cases
// where the without-bounds-checks macro is being used, but there is no
// %element or %element-setter defined for the collection.

define inline method %element
    (collection :: <collection>, key :: <object>)
 => (element :: <object>);
  element(collection, key);
end method %element;

define inline method %element-setter
    (new-value :: <object>, collection :: <mutable-collection>,
     key :: <object>)
 => (element :: <object>);
    element-setter(new-value, collection, key);
end method %element-setter;

define method size (collection :: <collection>) => res :: <integer>;
  for (element in collection,
       result :: <integer> from 0)
  finally
    result;
  end;
end method size;

define inline method empty? (coll :: <collection>) => res :: <boolean>;
  let (state, limit, ignore, finished-state?)
    = forward-iteration-protocol(coll);
  finished-state?(coll, state, limit);
end method empty?;

define function key-intersection
    (collection :: <collection>, more-collections :: <simple-object-vector>)
    => res :: <sequence>;
  let test = key-test(collection);
  let keys = key-sequence(collection);
  for (other-collection :: <collection> in more-collections)
    unless (other-collection.key-test == test)
      error("Can't do over collections with different key tests");
    end;
    keys := intersection(keys, key-sequence(other-collection), test: test);
  end;
  keys;
end function key-intersection;

define method do
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <false>;
  if (empty?(more-collections))
    for (element in collection)
      proc(element);
    end;
  else
    let keys = key-intersection(collection, more-collections);
    for (key in keys)
      apply(proc, collection[key],
	    map(method (coll) coll[key] end method, more-collections));
    end for;
  end;
  #f;
end method do;

define inline method map
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <collection>;
  apply(map-as, type-for-copy(collection), proc, collection,
	more-collections);
end method map;

// If the type specifier for the first argument is changed to
// subclass(<mutable-collection>), it breaks this method for
// limited collections. Need to investigate further.
//
define method map-as
    (type :: <type>, proc :: <function>, collection :: <collection>,
     #rest more-collections)
    => res :: <mutable-collection>;
  if (empty?(more-collections))
    let (init, limit, next-state, finished?, current-key, current-element)
      = forward-iteration-protocol(collection);
    let result = make-collection(type, collection.size);

    // We can just iterate normally across collection, but we can't
    // iterate across result because we don't know that
    // collection.key-sequence matches result.key-sequence

    for (state = init then next-state(collection, state),
	 until: finished?(collection, state, limit))
      result[current-key(collection, state)]
	:= proc(current-element(collection, state));
    end;
    result;
  else
    let keys = key-intersection(collection, more-collections);
    let result = make-collection(type, keys.size);
    for (key in keys)
      result[key]
	:= apply(proc, collection[key],
		 map(method (coll) coll[key] end method, more-collections));
    end for;
    result;
  end;
end method map-as;

define method map-into
    (target :: <mutable-collection>, proc :: <function>,
     collection :: <collection>, #rest more-collections)
    => res :: <mutable-collection>;
  let test = key-test(target);
  unless (collection.key-test == test)
    error("Can't do over collections with different key tests");
  end;
  let keys = intersection(key-sequence(target),
			  key-intersection(collection, more-collections),
			  test: test);
  // ### The book says that we are supposed to replace the target's elements
  // with the results of applying proc to the collections.  But how do we
  // flush out the old keys?
  for (key in keys)
    target[key]
      := apply(proc, collection[key],
	       map(method (coll) coll[key] end method, more-collections));
  end for;
  target;
end method map-into;

// make-collection -- internal
//
define inline method make-collection
    (type :: <type>, collection-size :: <integer>)
 => (res :: <mutable-collection>);
  make(type, size: collection-size);
end method;

define inline method make-collection
    (type :: subclass(<array>), collection-size :: <integer>)
 => (res :: <mutable-collection>);
  make(type, dimensions: vector(collection-size));
end method;

define inline method make-collection
    (type :: subclass(<vector>), collection-size :: <integer>)
 => (res :: <mutable-collection>);
  make(type, size: collection-size);
end method;

define inline method any?
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <object>;
  block (return)
    apply(do,
	  method (#rest args)
	    let res = apply(proc, args);
	    if (res)
	      return(res);
	    end;
	  end,
	  collection,
	  more-collections);
    #f;
  end block;
end method any?;

define inline method every?
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <boolean>;
  block (return)
    apply(do,
	  method (#rest args)
	    unless (apply(proc, args))
	      return(#f);
	    end;
	  end,
	  collection,
	  more-collections);
    #t;
  end block;
end method every?;

// reduce and reduce1 can't be inline, or the compiler transform for them
// won't trigger.
// 
define method reduce
    (proc :: <function>, init-value :: <object>, collection :: <collection>)
    => res :: <object>;
  for (res = init-value then proc(res, element),
       element in collection)
  finally
    res;
  end for;
end method reduce;

define method reduce1
    (proc :: <function>, collection :: <collection>)
    => res :: <object>;
  for (res = #f then if (first?) element else proc(res, element) end,
       first? = #t then #f,
       element in collection)
  finally
    if (first?)
      error("reduce1 must be passed at least one element.");
    else
      res;
    end if;
  end for;
end method reduce1;

define inline method choose
    (predicate :: <function>, sequence :: <sequence>)
 => (result :: <sequence>);
  for (result = #()
	 then if (predicate(elem)) pair(elem, result) else result end if,
       elem in sequence)
  finally
    as(type-for-copy(sequence), reverse!(result));
  end for;
end method choose;

define inline method choose-by
    (predicate :: <function>, test-seq :: <sequence>, value-seq :: <sequence>)
 => (result :: <sequence>);
  for (result = #() then if (predicate(test-elem))
			   pair(value-elem, result);
			 else
			   result;
			 end if,
       value-elem in value-seq,
       test-elem in test-seq)
  finally
    as(type-for-copy(value-seq), reverse!(result));
  end for;
end method choose-by;

define inline method member?
    (value :: <object>, collection :: <collection>, #key test :: <function> = \==)
    => res :: <boolean>;
  block (return)
    for (el in collection)
      if (compare-using-default-==(test, value, el))
	return(#t);
      end;
    end;
    #f;
  end;
end method member?;

define method find-key
    (collection :: <collection>, proc :: <function>,
     #key skip :: <integer> = 0, failure = #f)
 => (key-or-failure :: <object>);
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(collection);
  block (return)
    for (state = init-state then next-state(collection, state),
	 until: done?(collection, state, limit))
      if (proc(current-element(collection, state)))
	if (skip > 0)
	  skip := skip - 1;
	else
	  return(current-key(collection, state));
	end if;
      end if;
    finally
      failure
    end for;
  end block;
end method find-key;

define method replace-elements!
    (collection :: <mutable-collection>, predicate :: <function>,
     new-value-fn :: <function>, #key count :: false-or(<integer>))
    => collection :: <mutable-collection>;
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(collection);
  for (state = init-state then next-state(collection, state),
       until: done?(collection, state, limit) | count == 0)
    let this-element = current-element(collection, state);
    if (predicate(this-element))
      current-element(collection, state) := new-value-fn(this-element);
      if (count) count := count - 1 end if;
    end if;
  end for;
  collection;
end method replace-elements!;


define method fill!
    (collection :: <mutable-collection>, value :: <object>, #key)
 => collection :: <mutable-collection>;
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(collection);
  for (state = init-state then next-state(collection, state),
       until: done?(collection, state, limit))
    current-element(collection, state) := value;
  end for;
  collection;
end method fill!;

define inline method shallow-copy
    (collection :: <collection>) => (result :: <collection>);
  map(identity, collection);
end method shallow-copy;

define method key-sequence (collection :: <collection>)
    => keys :: <sequence>;
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(collection);
  let result = make(<vector>, size: size(collection));
  for (index :: <integer> from 0,
       state = init-state then next-state(collection, state),
       until: done?(collection, state, limit))
    result[index] := current-key(collection, state);
  end for;
  result;
end method key-sequence;

// No method on key-test for <collection>.


// Explicit-key-collection generics

define open generic remove-key!
    (table :: <mutable-explicit-key-collection>, key :: <object>)
 => (result :: <boolean>);


// Sequence generics

define open generic size-setter (new :: <object>, object :: <object>)
 => (new :: <object>);

define open generic add
    (seq :: <sequence>, new-element :: <object>) => (new-seq :: <sequence>);

define open generic add!
    (seq :: type-union(<sequence>, <set>), new-element :: <object>)
 => (maybe-new-seq :: type-union(<sequence>, <set>));

define open generic add-new
    (seq :: <sequence>, new-element :: <object>, #key test)
 => (new-seq :: <sequence>);

define open generic add-new!
    (seq :: <sequence>, new-element :: <object>, #key test)
 => (new-seq :: <sequence>);

define open generic remove
    (seq :: <sequence>, value :: <object>, #key test, count)
    => new-seq :: <sequence>;

define open generic remove!
    (seq :: type-union(<sequence>, <set>), value :: <object>, #key test, count)
 => (maybe-new-seq :: type-union(<sequence>, <set>));

define open generic choose
    (pred :: <function>, seq :: <sequence>)
    => new-seq :: <sequence>;

define open generic choose-by
    (pred :: <function>, test-seq :: <sequence>, value-seq :: <sequence>)
    => new-seq :: <sequence>;

define open generic intersection
    (seq1 :: <sequence>, seq2 :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic difference
    (seq1 :: <sequence>, seq2 :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic union
    (seq1 :: <sequence>, seq2 :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic remove-duplicates
    (seq :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic remove-duplicates!
    (seq :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic copy-sequence
    (source :: <sequence>, #key start, end: last)
    => new-seq :: <sequence>;

define sealed generic concatenate-as
    (type :: <type>, seq :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;

define sealed generic concatenate
    (seq :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;

define open generic concatenate!
    (sequence :: <sequence>, #rest more-sequences)
 => (result :: <sequence>);

define open generic replace-subsequence!
    (seq :: <sequence>, insert-seq :: <sequence>, #key start, end: last)
    => result-seq :: <sequence>;

define open generic reverse (seq :: <sequence>) => (new-seq :: <sequence>);

define open generic reverse! (seq :: <sequence>) => (new-seq :: <sequence>);

define open generic sort
    (seq :: <sequence>, #key test, stable)
    => new-seq :: <sequence>;

define open generic sort!
    (seq :: <sequence>, #key test, stable)
    => new-seq :: <sequence>;

define sealed generic first
    (seq :: <sequence>, #key default) => (value :: <object>);

define sealed generic second
    (seq :: <sequence>, #key default) => (value :: <object>);

define sealed generic third
    (seq :: <sequence>, #key default) => (value :: <object>);

define sealed generic first-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define sealed generic second-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define sealed generic third-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define open generic last
    (seq :: <sequence>, #key default) => (value :: <object>);

define open generic last-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define open generic subsequence-position
    (big :: <sequence>, pattern :: <sequence>, #key test, count)
 => (index :: false-or(<integer>));


// Sequence methods

define inline method add
    (seq :: <sequence>, new-element :: <object>) => (new-seq :: <sequence>);
  concatenate(seq, vector(new-element));
end method add;

define inline method add!
    (seq :: <sequence>, new-element :: <object>)
 => (maybe-new-seq :: <sequence>);
  add(seq, new-element);
end method add!;

define inline method add-new
    (seq :: <sequence>, new-element :: <object>, #key test :: <function> = \==)
 => (new-seq :: <sequence>);
  if (member?(new-element, seq, test: commutate(test)))
    seq;
  else
    add(seq, new-element);
  end if;
end method add-new;

define inline method add-new!
    (seq :: <sequence>, new-element :: <object>, #key test :: <function> = \==)
 => (new-seq :: <sequence>);
  if (member?(new-element, seq, test: commutate(test)))
    seq;
  else
    add!(seq, new-element);
  end if;
end method add-new!;

// author: PDH
define method remove
    (sequence :: <sequence>, value,
     #key test :: <function> = \==, count :: false-or(<integer>))
 => (result :: <sequence>);
  for (elem in sequence,
       result = #() then if ((count & (count <= 0))
                             | ~compare-using-default-==(test, elem, value))
                            pair(elem, result);
                         else
                           if (count) count := count - 1 end;
                           result;
                         end if)
  finally
    as(type-for-copy(sequence), reverse!(result));
  end for;
end method remove;

define inline method remove!
    (sequence :: <sequence>, value,
     #key test :: <function> = \==, count :: false-or(<integer>))
 => (result :: <sequence>);
  remove(sequence, value, test: test, count: count);
end method remove!;

define method reverse (sequence :: <sequence>) => (result :: <sequence>);
  let result = make(sequence.type-for-copy, size: sequence.size);
  let (res-state, res-limit, res-next, res-done?, res-key, res-elem,
       res-elem-setter) = forward-iteration-protocol(result);
  let (source-state, source-limit, source-next, source-done?, source-key,
       source-elem) = forward-iteration-protocol(sequence);
  local method reverse1(res-state, source-state) // :: res-state
	  if (source-done?(sequence, source-state, source-limit))
	    res-state
	  else 
	    let elem = source-elem(sequence, source-state);
	    let new-res-state =
	      reverse1(res-state, source-next(sequence, source-state));
	    res-elem(result, new-res-state) := elem;
	    res-next(result, new-res-state);
	  end if;
	end method reverse1;
  reverse1(res-state, source-state);
  result;
end method;

define inline method reverse! (sequence :: <sequence>)
 => (result :: <sequence>);
  reverse(sequence);
end method reverse!;

define method intersection
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test :: <function> = \==)
 => (result :: <sequence>);
  choose(method (item) member?(item, sequence2, test: test) end method,
	 sequence1);
end method intersection;

define method difference
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test :: <function> = \==)
 => (result :: <sequence>);
  choose(method (item) ~member?(item, sequence2, test: test) end method,
	 sequence1);
end method difference;

define method union
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test :: <function> = \==)
 => (result :: <sequence>);
  concatenate(sequence1, difference(sequence2, sequence1,
				    test: commutate(test)));
end method union;

define method remove-duplicates
    (sequence :: <sequence>, #key test :: <function> = \==)
 => (result :: <sequence>);
  let true-test = commutate(test);
  for (element in sequence,
       result = #() then if (member?(element, result, test: true-test))
                           result;
                         else
                           pair(element, result);
                         end if)
  finally
    as(type-for-copy(sequence), reverse!(result));
  end for;
end method remove-duplicates;

define inline method remove-duplicates!
    (sequence :: <sequence>, #key test :: <function> = \==)
 => (result :: <sequence>);
  remove-duplicates(sequence, test: test);
end method remove-duplicates!;

define method replace-subsequence!
    (sequence :: <sequence>, insert-sequence :: <sequence>,
     #key start: first :: <integer> = 0,
          end: last :: <integer> = sequence.size)
 => (sequence :: <sequence>);
  concatenate(copy-sequence(sequence, start: 0, end: first), insert-sequence,
	      copy-sequence(sequence, start: last));
end method replace-subsequence!;

define method subsequence-position
    (big :: <sequence>, pattern :: <sequence>,
     #key test :: <function> = \==, count :: <integer> = 1)
 => (result :: false-or(<integer>));
  if (empty?(pattern))
    0
  else
    let (init-state, limit, next-state, done?,
	 current-key, current-element,
	 current-element-setter, copy-state) = forward-iteration-protocol(big);
    let (pat-init-state, pat-limit, pat-next-state,
	 pat-done?, pat-current-key, pat-current-element,
	 pat-current-element-setter,
	 pat-copy-state) = forward-iteration-protocol(pattern);
    local method search(index, index-state, big-state, pat-state, count)
	    case
	      pat-done?(pattern, pat-state, pat-limit) =>
		// End of pattern -- We found one.
		if (count = 1)
		  index
		else
		  let next = next-state(big, index-state);
		  search(index + 1, next, copy-state(big, next),
			 pat-copy-state(pattern, pat-init-state), count - 1);
		end if;
	      done?(big, big-state, limit) =>
		// End of big sequence -- it's not here.
		#f;
	      compare-using-default-==(test, current-element(big, big-state),
		   pat-current-element(pattern, pat-state)) =>
		// They match -- try one more.
		search(index, index-state, next-state(big, big-state),
		       pat-next-state(pattern, pat-state), count);
	      otherwise =>
		// Don't match -- try one further along.
		let next = next-state(big, index-state);
	        search(index + 1, next, next & copy-state(big, next),
		       pat-copy-state(pattern, pat-init-state), count);
	    end case;
	  end method search;
    search(0, copy-state(big, init-state), copy-state(big, init-state),
	   pat-copy-state(pattern, pat-init-state), count);
  end if;
end method subsequence-position;

// author: PDH
// Returns the minimum of two sequence sizes or false if neither
// sequence is bounded.
//
define inline-only method min-size
    (sequence1 :: <sequence>, sequence2 :: <sequence>)
    => result :: false-or(<integer>);
  let size1 :: false-or(<integer>) = sequence1.size;
  let size2 :: false-or(<integer>) = sequence2.size;
  if (size1)
    if (size2)
      min(size1, size2);
    else
      size1;
    end if;
  else
    size2;
  end if;
end method min-size;

// author: PDH
// Returns the minimum size of the sequences in more-sequences,
// or false if none of the sequences is bounded. The first parameter
// acts as a seed.
//
define inline-only method min-size
    (len :: false-or(<integer>),
     more-sequences :: <rest-parameters-sequence>)
    => result :: false-or(<integer>);
  for (other-sequence :: <sequence> in more-sequences)
    let other-size :: false-or(<integer>) = other-sequence.size;
    if (other-size)
      len := if (len)
               min(len, other-size);
             else
               other-size;
             end;
    end if;
  end for;
  len;
end method min-size;

// author: PDH
// 10x speed-up over previous version of this method (not the compiler
// transform)
// d2c has a transform for visible calls to do (over sequences) which
// is much faster than the code below when the size of more-collections
// is greater than two.
//
define method do
    (proc :: <function>, sequence0 :: <sequence>, #rest more-collections)
    => res :: <false>;
  let sz = sequence0.size;

  let (sequence0-init, ignore1, sequence0-next-state, ignore2,
       ignore3, sequence0-current-element) = forward-iteration-protocol(sequence0);

  let more-size = more-collections.size;
  if (more-size == 0)
    unless (sz)
      unbounded-collection-error(do);
    end;
    for (index from 0 below sz,
         sequence0-state = sequence0-init then sequence0-next-state(sequence0, sequence0-state))
      proc(sequence0-current-element(sequence0, sequence0-state));
    end for;
  elseif (every?(rcurry(instance?, <sequence>), more-collections))
    let more-sequences = more-collections;

    let sz = min-size(sz, more-sequences);
    unless (sz)
      unbounded-collection-error(do);
    end;

    if (more-size == 1)
      let sequence1 = more-sequences[0];
      let (sequence1-init, ignore1, sequence1-next-state, ignore2,
           ignore3, sequence1-current-element) = forward-iteration-protocol(sequence1);
      for (index from 0 below sz,
           sequence0-state = sequence0-init then sequence0-next-state(sequence0, sequence0-state),
           sequence1-state = sequence1-init then sequence1-next-state(sequence1, sequence1-state))
         proc(sequence0-current-element(sequence0, sequence0-state),
              sequence1-current-element(sequence1, sequence1-state));
      end for;
    elseif (more-size == 2)
      let sequence1 = more-sequences[0];
      let (sequence1-init, ignore1, sequence1-next-state, ignore2,
           ignore3, sequence1-current-element) = forward-iteration-protocol(sequence1);
      let sequence2 = more-sequences[1];
      let (sequence2-init, ignore1, sequence2-next-state, ignore2,
           ignore3, sequence2-current-element) = forward-iteration-protocol(sequence2);
      for (index from 0 below sz,
           sequence0-state = sequence0-init then sequence0-next-state(sequence0, sequence0-state),
           sequence1-state = sequence1-init then sequence1-next-state(sequence1, sequence1-state),
           sequence2-state = sequence2-init then sequence2-next-state(sequence2, sequence2-state))
        proc(sequence0-current-element(sequence0, sequence0-state),
             sequence1-current-element(sequence1, sequence1-state),
             sequence2-current-element(sequence2, sequence2-state));
      end for;
    else
      // Stuff all the forward-iteration-protocol info into a bunch of
      // parallel vectors..
      //
      let states = make(<vector>, size: more-size);
      let next-states = make(<vector>, size: more-size);
      let current-elements = make(<vector>, size: more-size);

      for (seq keyed-by i in more-sequences)
        let (init, ignore1, next-state, ignore2, ignore3, current-element)
          = forward-iteration-protocol(seq);
        states[i] := init;
        next-states[i] := next-state;
        current-elements[i] := current-element;
      end for;

      // To call the mapped function on the n'th elements of the
      // sequences, we need to collect all the elements.
      //
      let more-elts = make(<vector>, size: more-size);

      for (index from 0 below sz,
           sequence0-state = sequence0-init then sequence0-next-state(sequence0, sequence0-state))
        for (seq keyed-by i in more-sequences, state in states,
             next-state in next-states, current-element in current-elements)
          more-elts[i] := current-element(seq, state);
          states[i] := next-state(seq, state);
        end for;
        apply(proc, sequence0-current-element(sequence0, sequence0-state), more-elts);
      end for;
    end if;
    #f;
  else
    next-method();
  end if;
end method do;

// If the type specifier for the first argument is changed to
// subclass(<mutable-sequence>), it breaks this method for
// limited collections. Need to investigate further.
//
define method map-as
    (type :: <type>, proc :: <function>, sequence :: <sequence>,
     #next next-method, #rest more-collections)
    => res :: <mutable-sequence>;
  if (subtype?(type, <sequence>)
      & every?(rcurry(instance?, <sequence>), more-collections))
    let sz = min-size(sequence.size, more-collections);
    unless (sz)
      unbounded-collection-error(map-as);
    end;
    sequence-map-into(make-collection(type, sz), proc, sequence,
                      more-collections);
  else
    next-method();
  end;
end method map-as;

define method map-into
    (target :: <mutable-sequence>, proc :: <function>,
     sequence :: <sequence>, #next next-method, #rest more-collections)
    => res :: <mutable-sequence>;
  if (every?(rcurry(instance?, <sequence>), more-collections))
    sequence-map-into(target, proc, sequence, more-collections);
  else
    next-method();
  end;
end method map-into;


// author: PDH
// 2x speedup
//
define function sequence-map-into
    (target :: <mutable-sequence>, proc :: <function>,
     sequence0 :: <sequence>, more-sequences :: <rest-parameters-sequence>)
    => res :: <mutable-sequence>;
  let sz = min-size(target, sequence0);

  let (target-init, ignore1, target-next-state, ignore2,
       ignore3, ignore4, target-current-element-setter)
    = forward-iteration-protocol(target);

  let (sequence0-init, ignore1, sequence0-next-state, ignore2,
       ignore3, sequence0-current-element) = forward-iteration-protocol(sequence0);

  let more-size = more-sequences.size;
  if (more-size == 0)
    unless (sz)
      unbounded-collection-error(map-into);
    end;
    for (index from 0 below sz,
         target-state = target-init then target-next-state(target, target-state),
         sequence0-state = sequence0-init then sequence0-next-state(sequence0, sequence0-state))
      target-current-element(target, target-state)
        := proc(sequence0-current-element(sequence0, sequence0-state));
    end for;
  else
    let sz = min-size(sz, more-sequences);
    unless (sz)
      unbounded-collection-error(map-into);
    end;

    if (more-size == 1)
      let sequence1 = more-sequences[0];
      let (sequence1-init, ignore1, sequence1-next-state, ignore2,
           ignore3, sequence1-current-element) = forward-iteration-protocol(sequence1);
      for (index from 0 below sz,
           target-state = target-init then target-next-state(target, target-state),
           sequence0-state = sequence0-init then sequence0-next-state(sequence0, sequence0-state),
           sequence1-state = sequence1-init then sequence1-next-state(sequence1, sequence1-state))
        target-current-element(target, target-state)
          := proc(sequence0-current-element(sequence0, sequence0-state),
                  sequence1-current-element(sequence1, sequence1-state));
      end for;
    elseif (more-size == 2)
      let sequence1 = more-sequences[0];
      let (sequence1-init, ignore1, sequence1-next-state, ignore2,
           ignore3, sequence1-current-element) = forward-iteration-protocol(sequence1);
      let sequence2 = more-sequences[1];
      let (sequence2-init, ignore1, sequence2-next-state, ignore2,
           ignore3, sequence2-current-element) = forward-iteration-protocol(sequence2);
      for (index from 0 below sz,
           target-state = target-init then target-next-state(target, target-state),
           sequence0-state = sequence0-init then sequence0-next-state(sequence0, sequence0-state),
           sequence1-state = sequence1-init then sequence1-next-state(sequence1, sequence1-state),
           sequence2-state = sequence2-init then sequence2-next-state(sequence2, sequence2-state))
        target-current-element(target, target-state)
          := proc(sequence0-current-element(sequence0, sequence0-state),
                  sequence1-current-element(sequence1, sequence1-state),
                  sequence2-current-element(sequence2, sequence2-state));
      end for;
    else
      // Stuff all the forward-iteration-protocol info into a bunch of
      // parallel vectors..
      //
      let states = make(<vector>, size: more-size);
      let next-states = make(<vector>, size: more-size);
      let current-elements = make(<vector>, size: more-size);

      for (seq keyed-by i in more-sequences)
        let (init, ignore1, next-state, ignore2, ignore3, current-element)
          = forward-iteration-protocol(seq);
        states[i] := init;
        next-states[i] := next-state;
        current-elements[i] := current-element;
      end for;

      // To call the mapped function on the n'th elements of the
      // sequences, we need to collect all the elements.
      //
      let more-elts = make(<vector>, size: more-size);

      for (index from 0 below sz,
           target-state = target-init then target-next-state(target, target-state),
           sequence0-state = sequence0-init then sequence0-next-state(sequence0, sequence0-state))
        for (seq keyed-by i in more-sequences, state in states,
             next-state in next-states, current-element in current-elements)
          more-elts[i] := current-element(seq, state);
          states[i] := next-state(seq, state);
        end for;
        target-current-element(target, target-state)
          := apply(proc, sequence0-current-element(sequence0, sequence0-state), more-elts);
      end for;
    end if;
  end if;
  target;
end function sequence-map-into;

define method find-key
    (sequence :: <sequence>, proc :: <function>, #key skip, failure = #f)
    => key-or-failure :: <object>;
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(sequence);
  block (return)
    for (elem in sequence,
	 key :: <integer> from 0)
      if (proc(elem))
	if (skip & skip > 0)
	  skip := skip - 1;
	else
	  return(key);
	end if;
      end if;
    finally
      failure
    end for;
  end block;
end method find-key;


define method fill!
    (sequence :: <mutable-sequence>, value :: <object>,
     #key start: first :: <integer> = 0,
          end: last :: false-or(<integer>))
 => (sequence :: <mutable-sequence>);
    
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(sequence);
  for (state = init-state then next-state(sequence, state),
       index :: <integer> from 0 below first,
       until: done?(sequence, state, limit))
  finally
    if (last)
      for (state = state then next-state(sequence, state),
	   index :: <integer> from index below last,
	   until: done?(sequence, state, limit))
	current-element(sequence, state) := value;
      end for;
    else
      for (state = state then next-state(sequence, state),
	   until: done?(sequence, state, limit))
	current-element(sequence, state) := value;
      end for;
    end if;
  end for;
  sequence;
end method fill!;

define inline method key-sequence
    (sequence :: <sequence>) => keys :: <range>;
  let s = size(sequence);
  if (s)
    range(from: 0, below: s);
  else
    range(from: 0);
  end if;
end method key-sequence;

define sealed inline method key-test
    (sequence :: <sequence>) => test :: <function>;
  \==;
end method key-test;

define inline method concatenate (sequence :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;
  apply(concatenate-as, sequence.type-for-copy, sequence, more-sequences);
end;

define inline method concatenate!
    (sequence :: <sequence>, #rest more-sequences)
 => (result :: <sequence>)
  apply(concatenate, sequence, more-sequences);
end method concatenate!;

define method concatenate-as (type :: <type>, sequence :: <sequence>,
			     #rest more-sequences) => result :: <sequence>;
  local
    method int-size(seq) => size :: <integer>;
      let sz = seq.size;
      unless (sz)
	error ("CONCATENATE-AS not applicable to unbounded sequences");
      end;
      sz;
    end;

  let length = for (total = 0 then total + seq.int-size,
		    seq in more-sequences)
	       finally total + sequence.int-size;
	       end for;
		 
  let result = make-collection(type, length);
  let (init-state, limit, next-state, done?, current-key, current-element,
       current-element-setter) = forward-iteration-protocol(result);

  let new-state = for (elem in sequence,
		       state = init-state then next-state(result, state))
		    current-element(result, state) := elem;
		  finally state;
		  end for;
  for (result-state = new-state
	 then for (elem in sequence,
		   state = result-state then next-state(result, state))
		current-element(result, state) := elem;
	      finally state;
	      end for,
       sequence in more-sequences)
  end for;
  result;
end method concatenate-as;

// first, second, third, last
// Note: Since a user library implementing element may be using a
// different value to indicate "not suppplied", we cannot blindly
// pass $not-supplied to element, otherwise element may interprete
// it as the default value.
//
/*
  Change the method below back to something like the original when 
  the following bug affecting "apply" is fixed:
  https://gauss.gwydiondylan.org/bugs/show_bug.cgi?id=7146
  The note above regarding $not-supplied can be removed then too.
*/
define inline method first
    (sequence :: <sequence>, #key default = $not-supplied)
 => value :: <object>;
  if (default == $not-supplied)
    sequence[0];
  else
    element(sequence, 0, default: default);
  end;
end;

define inline method first-setter
    (new-value :: <object>, sequence :: <mutable-sequence>)
 => (new-value :: <object>);
  sequence[0] := new-value;
end;

// See note above.
//
/*
  Change the method below back to something like the original when 
  the following bug affecting "apply" is fixed:
  https://gauss.gwydiondylan.org/bugs/show_bug.cgi?id=7146
  The note above regarding $not-supplied can be removed then too.
*/
define inline method second
    (sequence :: <sequence>, #key default = $not-supplied)
 => value :: <object>;
  if (default == $not-supplied)
    sequence[1];
  else
    element(sequence, 1, default: default);
  end;
end;

define inline method second-setter
    (new-value :: <object>, sequence :: <mutable-sequence>)
 => (new-value :: <object>);
  sequence[1] := new-value;
end;

// See note above.
//
/*
  Change the method below back to something like the original when 
  the following bug affecting "apply" is fixed:
  https://gauss.gwydiondylan.org/bugs/show_bug.cgi?id=7146
  The note above regarding $not-supplied can be removed then too.
*/
define inline method third
    (sequence :: <sequence>, #key default = $not-supplied)
 => value :: <object>;
  if (default == $not-supplied)
    sequence[2];
  else
    element(sequence, 2, default: default);
  end;
end;

define inline method third-setter
    (new-value :: <object>, sequence :: <mutable-sequence>)
 => (new-value :: <object>);
  sequence[2] := new-value;
end;

define method copy-sequence
    (sequence :: <sequence>, #key start :: <integer> = 0, end: last :: false-or(<integer>))
 => (result :: <sequence>);
  let last = check-start-end-bounds("copy-sequence", sequence, start, last);

  let sz :: <integer> = last - start;
  let result = make-collection(type-for-copy(sequence), sz);
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(sequence);

  for (index :: <integer> from 0 below start,
       state = init-state then next-state(sequence, state))
  finally
    let (res-init, res-limit, res-next, res-done?, res-key, res-elem, res-elem-setter)
         = forward-iteration-protocol(result);
    for (index :: <integer> from index below last,
         state = state then next-state(sequence, state),
         res-state = res-init then res-next(result, res-state))
      res-elem(result, res-state) := current-element(sequence, state);
    end for;
  end for;
  result;
end method copy-sequence;

// This is an optimization for shallow-copy for sequences. It is not
// strictly necessary, and maybe should be removed when map is optimized
// more fully.
// Oddly, copy-sequence and shallow-copy return different different types
// of collections for ranges. Refer to
//   http://www.gwydiondylan.org/books/drm/drm_101.html#MARKER-2-1691
//   http://www.gwydiondylan.org/books/drm/drm_102.html#MARKER-2-1796
//
define inline method shallow-copy (sequence :: <sequence>)
 => result :: <sequence>;
  if (instance?(sequence, <range>))
    next-method();
  else
    copy-sequence(sequence);
  end if;
end method shallow-copy;

// See note above.
//
/*
  Change the method below back to something like the original when 
  the following bug affecting "apply" is fixed:
  https://gauss.gwydiondylan.org/bugs/show_bug.cgi?id=7146
  The note above regarding $not-supplied can be removed then too.
*/
define inline method last
    (sequence :: <sequence>, #key default = $not-supplied)
 => value :: <object>;
  let last-index :: <integer> = sequence.size - 1;
  if (default == $not-supplied)
    sequence[last-index];
  else
    element(sequence, last-index, default: default);
  end;
end method last;

define inline method last-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => value :: <object>;
  element(seq, seq.size - 1) := new-value;
end method last-setter;

// Note: This function depends upon a definition of \= for sequences, which
// will be supplied later in this file.
//
define method \= (a :: <collection>, b :: <collection>) => answer :: <boolean>;
  let a-test = key-test(a);
  let b-test = key-test(b);
  
  a-test == b-test
    & key-sequence(a) = key-sequence(b) 
    & every?(a-test, a, b);
end method \=;

define method \= (a :: <sequence>, b :: <sequence>) => answer :: <boolean>;
  let (a-init, a-limit, a-next, a-done?, a-key, a-elem)
    = forward-iteration-protocol(a);
  let (b-init, b-limit, b-next, b-done?, b-key, b-elem)
    = forward-iteration-protocol(b);
  block (return)
    for (a-state = a-init then a-next(a, a-state),
	 b-state = b-init then b-next(b, b-state),
	 until: a-done?(a, a-state, a-limit) | b-done?(b, b-state, b-limit))
      if (a-elem(a, a-state) ~= b-elem(b, b-state))
	return(#f);
      end if;
    finally
      if (~a-done?(a, a-state, a-limit) | ~b-done?(b, b-state, b-limit))
	return(#f);
      end if;
    end for;
    #t;
  end block;
end method \=;

// Key-exists -- Exported
//
// If the given key is present in the collection, return #t and the value
// associated with the key.  Otherwise, return #f and an undefined value.
//
// Can't use $not-supplied, because we're passing undefined as an
// argument to element(), which itself probably uses $not-supplied..
//
define constant undefined = pair(#f, #f);

define method key-exists? (coll :: <collection>, key :: <object>)
 => (result :: <boolean>, value :: <object>);
  let value = element(coll, key, default: undefined);
  values(value ~= undefined, value);
end method key-exists?;

// author: PDH
// A utility function for sequence operations which take start: and end: 
// keyword parameters
//
define inline function check-start-end-bounds
     (method-name :: <string>, sequence :: <sequence>, start :: <integer>, last :: false-or(<integer>))
  => last :: <integer>;
  // This type check will fail for unbounded sequences.
  let seq-size :: <integer> = sequence.size;
  let last :: <integer> = last | seq-size;
  if ((last > seq-size) | (start < 0) | (start > last))
    check-start-end-bounds-error(method-name, sequence, start, last);
  end if;
  last;
end function;

// out-of-line error function to minimize calling code size
// used throughout the runtime library
//
define not-inline function element-error (collection :: <collection>, index :: <integer>)
 => res :: <never-returns>;
  error("No element with index %d in %=", index, collection);
end function;

define not-inline function unbounded-collection-error (function :: <function>)
 => res :: <never-returns>;
  error("Function/method %= was expecting at least one bounded collection in its parameter list.", function);
end function;

// author: PDH
// The out-of-line error function for the check-start-end-bounds function
//
define not-inline function check-start-end-bounds-error
     (method-name :: <string>, sequence :: <sequence>, start :: <integer>, last :: <integer>)
  => (result :: <never-returns>)
  let seq-size :: <integer> = sequence.size;
  case
    (last > seq-size)
      => error("%s called on %= with end: (%=) index greater than sequence size (%=).", method-name, sequence, last, seq-size);
    (start < 0)
      => error("%s called on %= with start: (%=) index less than 0.", method-name, sequence, start);
    (start > last)
      => error("%s called on %= with start: (%=) > end: (%=).", method-name, sequence, start, last);
     otherwise
      => error("We should have never made it to here.");
  end case;
end function;

// author: PDH
// This is an optimization for functions who take a function parameter
// which defaults to \==. It eliminates a general entry for the case 
// where the comparison function is \== and does not hurt speed 
// measurably for cases where another comparison function is specified.
// Note: The return type must be <object>, not <boolean> because test
// may not return a <boolean>. (Of course if we omitted the return
// parameters, it wouldn't hurt anything because this function is 
// inline-only.)
//
define inline-only function compare-using-default-==
    (test :: <function>, a, b)
 => (object :: <object>)
  if (test == \==)
    a == b;
  else
    test(a, b);
  end;
end;

// Return a method the same as that passed in, but with the first
// two parameters reversed. However, if the test method is the
// identity function, just return it.
//
define inline-only function commutate (test :: <function>)
 => (reversed-test :: <function>)
  if (test == \==)
    \==;
  else
    method (a, b) test(b, a) end method;
  end;
end;

// %swap-elements! -- internal
//
// Swaps two elements in a vector without doing a bounds
// check of the indices.
//
define inline function %swap-elements!
    (vec :: <vector>, key1 :: <integer>, key2 :: <integer>)
 => ();
    let element1 = %element(vec, key1);
    let element2 = %element(vec, key2);
    %element(vec, key1) := element2;
    %element(vec, key2) := element1;
end function %swap-elements!;

// swap-elements! -- internal
//
// Swaps two elements in a vector.
//
define inline function swap-elements!
    (vec :: <vector>, key1 :: <integer>, key2 :: <integer>)
 => ();
  let element1 = vec[key1];
  let element2 = vec[key2];
  vec[key1] := element2;
  vec[key2] := element1;
end function swap-elements!;
