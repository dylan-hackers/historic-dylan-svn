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

// Lists and pairs
//
// Seals for most collection operations on the built-in collections can be
// found in seals.dylan.  Some exceptions apply, such as "make" and "as".
// See seals.dylan for more info.
//

/*
  Warning:  Though a <pair> is a <list>, many of the methods applicable to 
  lists expect a proper list, i.e. a list terminated by #(). These methods
  may work incorrectly for non-proper lists (may or may not causing an 
  error).

  Non-proper lists include:
    * a linked-list of one or more pairs where the tail of the last pair is 
      not an instance of <list>, i.e. not a <pair> or #()
    * a circular list: i.e. a linked-list of one or more pairs where the
      tail of the "last" pair is a previous pair (or the "last" pair itself)

  The DRM does not touch on the expected behavior of methods such as
  \=, add!, remove!, or size for non-proper lists (except that size is
  defined for circular lists). The whole <list>, <pair>, <empty-list>
  heirarchy is perhaps unfortunate.
*/

define abstract class <list> (<mutable-sequence>)
  sealed slot head :: <object>, setter: %head-setter,
    required-init-keyword: head:;
  sealed slot tail :: <object>, setter: %tail-setter,
    required-init-keyword: tail:;
end;

define sealed method make
    (class == <list>, #key size :: <integer> = 0, fill = #f)
 => (res :: <list>);
  for (i :: <integer> from 0 below size,
       result = #() then pair(fill, result))
  finally
    result;
  end;
end;

define class <empty-list> (<list>)
end;

define sealed method make (class == <empty-list>, #key)
    => res :: <empty-list>;
//  error("Can't make new instances of <empty-list>, #() is it.");
  #()
end;

define class <pair> (<list>)
end;

define sealed domain make (singleton(<pair>));

define inline function head-setter (new, pair :: <pair>) => new;
  pair.%head := new;
end;

define inline function tail-setter (new, pair :: <pair>) => new;
  pair.%tail := new;
end;

define inline method forward-iteration-protocol (list :: <list>)
    => (initial-state :: <list>,
	limit :: <list>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(list,
	 #(),
	 method (list :: <list>, state :: <list>)
	   state.tail;
	 end,
	 method (list :: <list>, state :: <list>, limit :: <list>)
	   state == limit;
	 end,
	 method (list :: <list>, state :: <pair>)
	   block (return)
	     for (key :: <integer> from 0,
		  pair :: <pair> = list then pair.tail,
		  until: pair == state)
	     finally
	       key;
	     end;
	   end;
	 end,
	 method (list :: <list>, state :: <pair>)
	   state.head;
	 end,
	 method (new-value :: <object>, list :: <list>, state :: <pair>)
	   state.head := new-value;
	 end,
	 method (list :: <list>, state :: <list>)
	   state;
	 end);
end;

// author: PDH, 1.2x speedup
// This method will work on unbounded lists (though not on lists 
// terminated with a non-list). It assumes that the tail of the
// empty list is the empty list. If the index is >= the size of 
// the list, this method will do some extra work by repeatedly
// calling tail on the empty list. That is the rare case though
// and unless the user specifies a default value, an error would 
// be signalled anyway. If this behavior is undesirable, a block
// could be added in. Be sure to run the TestWorks QA tests though.
//
define sealed method element
    (list :: <list>, index :: <integer>, #key default = $not-supplied)
 => (element :: <object>);
  let (found?, value) =
    if (index < 0)
      values(#f, #f)
    else
      for (li :: <list> = list then li.tail,
           current-index from 0 below index)
        finally
          if (empty?(li))
            values(#f, #f);
          else
            values(#t, li.head);
          end if;
      end for;
    end if;
  if (found?)
    value;
  elseif (default == $not-supplied)
    element-error(list, index);
  else
    default;
  end if;
end method element;

// author: PDH, 1.2x speedup
// This method will work on unbounded lists (though not on lists 
// terminated with a non-list). It assumes that the tail of the
// empty list is the empty list. If the index is >= the size of 
// the list, this method will do some extra work by repeatedly
// calling tail on the empty list. That is the rare case though
// and an error would be signalled anyway. If this behavior is 
// undesirable, revert to the original code which checks for an
// empty list initially and checks during each iteration.
//
define sealed method element-setter
    (new-value :: <object>, list :: <list>, index :: <integer>)
 => (new-value :: <object>);
    if (index < 0)
      element-error(list, index);
    else
      for (li :: <list> = list then li.tail,
           current-index from 0 below index)
        finally
          if (empty?(li))
            element-error(list, index);
          else
            li.head := new-value;
          end if;
      end for;
    end if;
end method element-setter;

// author: PDH
define sealed inline method first
    (list :: <list>, #key default = $not-supplied)
 => (element :: <object>);
  if (~empty?(list))
    list.head;
  elseif (default == $not-supplied)
    element-error(list, 0);
  else
    default;
  end if;
end method;

// author: PDH
define sealed inline method first-setter
    (new-value :: <object>, list :: <list>)
 => (new-value :: <object>);
  if (empty?(list))
    element-error(list, 0);
  else
    list.head := new-value;
  end if;
end method;

// author: PDH, 2x speedup
define sealed method last
    (list :: <list>, #key default = $not-supplied)
 => (element :: <object>);
  if (empty?(list))
    if (default == $not-supplied)
      error("list %= has no last because it is empty", list);
    else
      default;
    end if;
  else
    let next :: <list> = list.tail;
    if (empty?(next))
      list.head;
    else
      block (return)
        for (slow :: <list> = next then slow.tail, 
             fast :: <list> = next.tail then next.tail)
            if (slow == fast)
              error("list %= is circular and has no last", list);
            elseif (empty?(fast))
              return(next.head);
            else
              next := fast.tail;
              if (empty?(next))
                return(fast.head);
              end if;
            end if;
        end for;
      end block;
    end if;
  end if;
end method last;

// author: PDH, 2x speedup
define sealed inline method last-setter
    (new-value :: <object>, list :: <list>)
 => (new-value :: <object>);
  if (empty?(list))
    error("list %= has no last because it is empty", list);
  else
    let next :: <list> = list.tail;
    if (empty?(next))
      list.head := new-value;
    else
      block (return)
        for (slow :: <list> = next then slow.tail, 
             fast :: <list> = next.tail then next.tail)
            if (slow == fast)
              error("list %= is circular and has no last", list);
            elseif (empty?(fast))
              next.head := new-value;
              return();
            else
              next := fast.tail;
              if (empty?(next))
                fast.head := new-value;
                return();
              end if;
            end if;
        end for;
      end block;
    end if;
  end if;
  new-value;
end method last-setter;

define flushable inline function pair (head, tail)
    => res :: <pair>;
  make(<pair>, head: head, tail: tail);
end;

define flushable inline function list (#rest args)
    => res :: <list>;
  as(<list>, args);
end;

// Warning: This method will blow the stack for circular lists
//
define method shallow-copy (list :: <list>) => res :: <list>;
  local method dup-if-pair (object) => res;
	  if (instance?(object, <pair>))
	    pair(object.head, dup-if-pair(object.tail));
	  else
	    object;
	  end;
	end;
  dup-if-pair(list);
end method shallow-copy;

define flushable sealed method as
    (class == <list>, collection :: <collection>)
    => res :: <list>;
  for (results = #() then pair(element, results),
       element in collection)
  finally
    reverse!(results);
  end;
end;

define inline method as (class == <list>, list :: <list>)
    => res :: <list>;
  list;
end;

define flushable method as
    (class == <list>, vec :: <simple-object-vector>)
    => res :: <list>;
  for (index :: <integer> from (vec.size - 1) to 0 by -1,
       res = #() then pair(%element(vec, index), res))
  finally
    res;
  end;
end;

// It is important to define this method because the type-for-copy
// of an <empty-list> is <list>, not <empty-list>.
//
define inline method type-for-copy (object :: <list>)
 => (class :: <class>)
  <list>;
end;

define inline method empty? (list :: <list>) => res :: <boolean>;
  list == #();
end;

define inline method add (list :: <list>, element)
    => res :: <pair>;
  pair(element, list);
end;

define inline method add! (list :: <list>, element)
    => res :: <pair>;
  pair(element, list);
end;

// author: PDH
// This is essentially a specialized clone of the general method for sequences
define method remove
    (list :: <list>, value,
     #key test :: <function> = \==, count :: false-or(<integer>))
 => (result :: <list>);
  for (elem in list,
       result = #() then if ((count & (count <= 0))
                             | ~compare-using-default-==(test, elem, value))
                            pair(elem, result);
                         else
                           if (count) count := count - 1 end;
                           result;
                         end if)
  finally
    reverse!(result);
  end for;
end method remove;

// author: PDH
define method remove!
    (list :: <list>, value,
     #key test :: <function> = \==, count :: false-or(<integer>))
 => (result :: <list>);
  // This is tricky: Use #() to stand for #f
  let previous :: <list> = #();
  let next :: <list> = list;

  for (current :: <list> = list then next,
       until: empty?(current) | (count & (count <= 0)))
    next := current.tail;
    if (compare-using-default-==(test, current.head, value))
      if (previous == #())
        list := next;
      else
        previous.tail := next;
      end;
      if (count) count := count - 1 end;
    else
      previous := current;
    end if;
  end for;
  list;
end method remove!;

define sealed method concatenate!
    (pair :: <pair>, #next next-method, #rest more-sequences)
 => (pair :: <pair>)
  if(every?(rcurry(instance?, <list>), more-sequences))
    let prev :: <pair>
      = for(prev :: <pair> = pair then pair.tail,
            until: prev.tail == #())
        finally
          prev;
        end;
    for(lst :: <list> in more-sequences)
      unless(empty?(lst))
        prev.tail := lst;
        for(nprev :: <pair> = lst then nprev.tail,
            until: nprev.tail == #())
        finally
          prev := nprev;
        end;
      end unless;
    end for;
        
    pair;
  else
    next-method();
  end if;
end method concatenate!;

// author: PDH, 1.2x speedup
define method size (list :: <list>)
    => res :: false-or(<integer>);
  if (empty?(list))
    0;
  else
    let next :: <list> = list.tail;
    if (empty?(next))
      1;
    else
      block (return)
        for (slow :: <list> = next then slow.tail, 
             fast :: <list> = next.tail then next.tail,
             result from 2 by 2)
            if (slow == fast)
              return(#f);
            elseif (empty?(fast))
              return(result);
            else
              next := fast.tail;
              if (empty?(next))
                return(result + 1);
              end if;
            end if;
        end for;
      end block;
    end if;
  end if;
end method size;

define flushable method reverse (list :: <list>) => res :: <list>;
  for (results = #() then pair(element, results),
       element in list)
  finally
    results;
  end;
end;

define method reverse! (list :: <list>) => res :: <list>;
  let temp :: <list> = #();
  for (remaining :: <list> = list then temp,
       results :: <list> = #() then remaining,
       until: remaining == #())
    temp := remaining.tail;
    remaining.tail := results;
  finally
    results;
  end;
end;


define sealed method \= (list1 :: <list>, list2 :: <list>)
    => res :: <boolean>;
  if (list1 == list2)
    #t;
  elseif (list1 == #() | list2 == #())
    #f;
  else
    block (return)
      for (l1 = list1 then l1.tail,
	   l2 = list2 then l2.tail,
	   while: (instance?(l1, <pair>) & instance?(l2, <pair>)))
	unless (l1.head = l2.head) return(#f) end unless;
      finally
	l1 = l2;
      end for;
    end block;
  end if;
end method;

define sealed method \= (list :: <list>, sequence :: <sequence>)
    => res :: <boolean>;
  block (return)
    for (remaining = list then remaining.tail,
	 object in sequence)
      if (~instance?(remaining, <pair>))
	return(#f);
      elseif (remaining.head ~= object)
	return(#f);
      end;
    finally
      remaining == #();
    end;
  end;
end;

define sealed inline method \= (sequence :: <sequence>, list :: <list>)
    => res :: <boolean>;
  list = sequence;
end;

define sealed method union
    (list1 :: <list>, list2 :: <list>,
     #key test :: <function> = \==)
 => (result :: <list>);
  for (item in list1,
       result :: <list> = list2
	 then if (member?(item, list2, test: test))
		result;
	      else
		pair(item, result);
	      end if)
  finally
    result;
  end for;
end method;

define sealed method intersection
    (list1 :: <list>, list2 :: <list>,
     #key test :: <function> = \==)
 => (result :: <list>);
  for (item in list1,
       result :: <list> = #()
	 then if (member?(item, list2, test: test))
		pair(item, result);
	      else
		result;
	      end if)
  finally
    result;
  end for;
end method;
