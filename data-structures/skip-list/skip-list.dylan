language: infix-dylan
module: skip-list

//==============================================================================
//
// Copyright (c) 1996 by Kai W. Zimmermann, Hamburg, Germany
// All rights reserved
//
// This code is provided as is for non-commercial, non-professional and 
// non-military use.  No warranty is taken for any consequences that may
// happen due to the use of this code, be it loss of software, hardware, 
// money, time, friends, etc..  You may use and distribute this code
// freely as long as this notice stays intact.  If you use this code in 
// your projects, please mention me in the documentation.  If you 
// redistribute this software you may not charge a fee for it.   
// 
// If you make changes to the code, port it to another platform, or use it
// in a major project, please let me know.  
// 
//==============================================================================

//==============================================================================
//
// Version 1.0
//
// Author
// 
// KWZ       =    Kai W. Zimmermann, Hamburg, Germany
//                zimmerma@informatik.uni-hamburg.de
// DJV       =    Dustin Voss, Seattle, United States
//                d_j_v@mac.com, |Agent on irc://irc.freenode.net/#ylan
//
// History
//
// 17.03.1996  KWZ  Release of version 1.0
//                  Uploaded the file to the Gwydion FTP server.
// 15.03.1996  KWZ  Tail recursive reformulation of the element access functions
//                  Provided type coercion with as
// 13.03.1996  KWZ  Provided <collection> protocols
// 23.01.1996  KWZ  Created in Apple Dylan TR
// 08.03.2009  DJV  Modified for Gwydion Dylan and Open Dylan.
//                  Added iteration independent of key order.
//                  Replaced $empty-skip-list-node with #f.
//                  Added print-object implementation.
//
//==============================================================================

/*
================================================================================

Description
 
A skip-list is a data type equivalent to a balanced (binary) tree. All keys must
be comparable by some kind of ordering function, e.g., <.

The data structure looks like this. You start searching for a key in the highest
level of header H, taking big steps along the list and then descend to the one
step level. Example: looking for 6, start at H, highest level. Find 7. Descend,
because 7>6. Find 3. Move on, since 3<6. Find 7. Descend, because 7>6. Find 5.
Move on, since 5<6. Find 7, for the last time now :-). Descend. Find 6.

H
o ------------------------------------> o -----------> NIL  Level 4  Next[3]
o ----------------> o ----------------> o -----------> NIL  Level 3  Next[2]
o ------> o ------> o ------> o ------> o ------> o -> NIL  Level 2  Next[1]
o -> o -> o -> o -> o -> o -> o -> o -> o -> o -> o -> NIL  Level 1  Next[0]
     0    1    2    3    4    5    6    7   123  424        <- Keys
     A    X    k    G    U    d    p    q    W    B         <- Values

The expensive part, equivalent to balancing, is to find the corresponding level
for each node. Therefore a probabilistic alternative is implemented, where a new
level is chosen at random, whenever a node is added. The performance is
comparable to a probabilistically balanced binary tree.

Each node has a second set of pointers for use during forward or backward
iteration.

H                                                              H
o -----> o -> o -> o -> o -> o -> o -> o -> o -> o -> o -> NIL    Forward
  NIL <- o <- o <- o <- o <- o <- o <- o <- o <- o <- o <----- o  Backward
         5    2    6    7    0   424   3    4   123   1           <- Keys
         d    k    p    q    A    B    G    U    W    X           <- Values

For details see:
  W. Pugh (1990) "Skip Lists: A Probabilistic Alternative to Balanced Trees."
  Communications of the ACM 33 (6): 668-676

I tried to make the library (hash)table like. 

Create a skip-list, e.g.,       define variable H = make(<skip-list>);
Now add the elements            H[0] := "A"; H[424] := "B"; H[3] := "G";   
You can look them up via        H[3]
Map over them with              for (x in H) signal("%s ", x) end;
Map over them in key order      for (x in H using
                                     forward-by-key-iteration-protocol)
Get rid of a key with           remove-key!(H, 3);
Reorder elements with           H.element-sequence := sort(H.element-sequence);
Of course you can use any key and not only integers, as long as you provide an
ordering function:              make(<skip-list>, test: case-insensitive-equal?,
                                     key-order: case-insensitive-less?)

Some test functions show that Pugh is right. There is very little difference in
the timing for p=1/2 and p=1/4, even 1/5 is fine. The lower the probability, the
less space you need, e.g., p=1/2 in theory uses twice as much pointers as p=1/4.
E.g., with p=1/4 and 10000 elements you need about 24 key comparisons to look up
one value. The measures are about the same for p=1/2, but the maximum level used
- and therefore the number of pointers - is about two times higher. A totally
balanced binary tree would need about 14 comparisons. The make function takes
max-level:, probability:, size:, and level: to adjust performance.

This module is intended as an extension to the collection library and inspired
by the extensions from the Gwydion Project at Carnegie Mellon University. In
addition, it's my first try on Dylan :-) If you have any tips concerning style,
efficiency, etc., please let me know.

The code depends on the availability of a RANDOM function. For portability I
used the one from the Gwydion Project at Carnegie Mellon University. That is not
very efficient, because a lot of number conversions occur. There is another in
the Mac toolbox.

I had to reformulate the algorithms from iterative to tail recursive versions,
due to some feature (design flaw?) of the Apple Dylan TR. In Apple's Dylan TR
the use of := on a local variable creates 8 byte garbage per variable. Rebinding
it using let or tail recursion does not use extra space. Just move the comment
delimiters to revert to iterative, if you think that is necessary.

I hope this code is of use to someone.
Kai

================================================================================
*/

//==============================================================================
//
// Notes
//
// * There seems to be an error.  remove-key! is documented to be 
//   "open generic", pp. 324.  In Apple Dylan TR I get an error message, 
//   but it works?  Comments highly welcome.
// 
//   ERROR MESSAGE
//   Sealing violation in method for imported sealed generic function 
//   "remove-key!".  The sealed generic function "remove-key!" is defined in 
//   library "Dylan".  The method definition {method of generic function 
//   remove-key! (x :: <basic-skip-list>, skey)}" is invalid because methods 
//   cannot be defined for a sealed generic function outside the library that 
//   defines the generic function. The compiler may generate optimized code 
//   that assumes that this method is not present. You must eliminate this 
//   method or else change the library "Dylan" to define "remove-key!" with 
//   define open generic. 
//
// * No = initialization in primary classes?  It seems one has to use the 
//   init-value keyword to initialize slots in primary classes.  Is that 
//   standard Dylan?  (See the end of file Tests.dylan)
//
// * This files was created by exporting from the TR.
//   If you have problems using it, please let me know.
//
//==============================================================================


// Implementation


define constant <next-node-vector> =
    limited(<simple-vector>, of: type-union(<skip-list-node>, <skip-list>,
                                            singleton(#f)));


define primary class <basic-skip-list> 
  (<stretchy-collection>, <mutable-explicit-key-collection>)
  // If you use this class directly or implement your own subclasses,
  // see the notes at flush-cache
  
  // PUBLIC
  
  // slot accessor provides method for standard collection op "key-test"
  slot key-test :: <function>, 
     init-value: $default-skip-list-test, 
     setter: #f,
     init-keyword: test:;
  
  // The user can provide a key comparison function 
  slot key-order :: <function>,
     init-value: $default-skip-list-order,
     setter: #f,
     init-keyword: key-order:;
  
  // The level of this skip-list wont grow past this limit, default = 32, 
  // i.e., skip-lists containing more than 2^32 elements might be stored 
  // less efficiently.  If you provide the init-keyword level: with the 
  // same value as max-level, you get a static level and a little less
  // garbage is produced during building.
  slot max-level :: <integer>,
     init-value: $default-skip-list-max-level,
     init-keyword: max-level:;
  
  // The probability to create a new level is 1/fan-out, 
  // controls the fan-out of the equivalent tree.
  // Pugh suggests the value of 1/4 for a good space/time trade-off
  slot probability :: <number>,
     init-value: 0.25,
     init-keyword: probability:;
  
  // PRIVATE
  
  // KWZ 13.03.1996  for efficiency
  slot private-size :: <integer>,
     init-value: 0;
  
  // Next holds the references to the actual <skip-list-nodes>. The nodes are
  // ordered by key.
  slot next :: <next-node-vector>, init-value: make(<next-node-vector>);
  
  // DJV 08.03.2009
  // Forward and backward reference the first and last nodes returned by
  // forward- or backward-iteration-protocol.
  slot forward :: false-or(<skip-list-node>), init-value: #f;
  slot backward :: false-or(<skip-list-node>), init-value: #f;
  
  // The actual maximum level used, cached for efficiency
  slot level :: <integer>,
     init-value: 1;
  
  // In a multi-threaded environment provisions must be taken to insure
  // that only one process accesses this slot at one time.  The easiest way 
  // to achieve this is to make it an instance slot.
  slot skip-list-update-cache,
     init-value: make(<next-node-vector>,
                      size: $default-skip-list-max-level,
                      fill: #f);

end class;


define constant $default-skip-list-test = \==;


define constant $default-skip-list-order = \<;


define constant $default-skip-list-max-level = 32;


// DJV 08.03.2009  added size keyword and guess-level generic as hint
define method initialize (sl :: <basic-skip-list>,
                          #key size :: false-or(<integer>),
                               level: init-level :: false-or(<integer>));
  // If you provide the size: init-keyword, the level will be preset to an
  // appropriate value, providing some small degree of efficiency.

  next-method();
  let init-level = init-level | guess-level(size);
  sl.level := min(sl.max-level, max(1, init-level));
  sl.next := make(<next-node-vector>, size: sl.level, fill: #f);
end method initialize;


define method guess-level (size == #f)
  // If starting level is unspecified, stick with slot init-value.
  1
end method;


define method guess-level (size :: <integer>)
  // I don't have an implementation-independent way to count bits, which would
  // be the fastest solution.
  ceiling(logn(size, 2));
end method;


define method random-level (skip :: <basic-skip-list>);
  // Return a random level number
  // The probability to return k is exp(prob,k),
  // i.e. it decreases exponentially
  // Taken from Pugh, pp. 670
  
  let max = skip.max-level;
  let prob = round/(1, skip.probability);
  let nlevel = 1;
  
  while ((random(prob) < 1) &
         (nlevel < max))
    nlevel := nlevel + 1;
  end;
  
  nlevel;
end method;


define method clear-cache (sl :: <basic-skip-list>);
  // The same update vector is used for all skip-lists for efficiency.
  // This could lead to the undesired effect that the update-cache is the
  // only reference to some skip-list, so it does not get garbage collected.  
  // To eliminate these unwanted references call clear-cache.  To be on the  
  // safe side, use the class <skip-list>, which calls clear-cache after each 
  // add or remove operation.  In Apple Dylan you could provide some specialized
  // garbage collection behavior, too, e.g., always flush the cache prior 
  // to gc.  If you want to build your own optimized subclass of 
  // <basic-skip-list>, remember to clear the cache on occasion.

  let update = sl.skip-list-update-cache;
  for (i from 0 below update.size,
       while: update[i])
    // set each field to NIL
    update[i] := #f;
  end;
  #t;
end;


define method find-key-and-maintain-update (sl :: <basic-skip-list>, skey);
  // Find the node corresponding to skey
  
  // Remember which pointers must be changed
  let update = sl.skip-list-update-cache;
  
  // Cache the collection specific comparison functions
  let key<  = sl.key-order;
  
  // Iterative Version
  // Taken from Pugh, pp. 670
  // This produces 8 bytes garbage each call with Apple TR, since := is used
  
  let x = sl;
  for (i from x.level - 1 to 0 by -1)
    while (x.next[i] & key<(x.next[i].key, skey))
      x := x.next[i];
    end while;
    update[i] := x;
  end for;
  
  // x.next[0] is #f OR
  // x.key < skey <= x.next[0].key
  x.next[0];

  /*
  // Tail recursive reformulation, no garbage is produced
  
  local method for-loop (x, i :: <integer>)
          // Decrement the level
          if (i < 0)
            x.next[0]
          else
            let last-node = while-loop(x, i);
              update[i] := last-node;
              for-loop(last-node, i - 1)
          end;
        end for-loop,
        
        method while-loop (x, i :: <integer>)
          // Follow pointer chain on one level
          if (x.next[i] & key<(x.next[i].key, skey))
            while-loop(x.next[i], i);
          else
            x
          end;
        end while-loop;
  
  // result
  for-loop(sl, sl.level - 1);
  */
 end method;


//==============================================================================
//
// For external use
//
//==============================================================================


define method element (sl :: <basic-skip-list>, skey :: <object>, 
                       #key default = unsupplied())
  => elem :: <object>;            
  // Return the value associated with skey, if skey is in the table
  
  // Cache the collection specific comparison functions
  let key== = sl.key-test;
  let key<  = sl.key-order;
  
  // Taken from Pugh, pp. 669
  // Old Iterative Version, since := is used on a local variable,
  // each call to element produces 8 bytes garbage in Apples TR
  let x = sl;
  for (i from x.level - 1 to 0 by -1)
    while (x.next[i] & key<(x.next[i].key, skey))
      x := x.next[i];
    end while;
  end for;

  // x.next[0] is $empty-skip-list-node OR
  // x.key < skey <= x.next[0].key
  
  let x = x.next[0];
  
  /*
  // Tail recursive reformulation, no garbage is produced
  
  local method for-loop (x, i :: <integer>)
          if (i < 0)
            // x.next[0] is #f OR
            // x.key < skey <= x.next[0].key
            // Return next node
            x.next[0]
          else
           // Follow pointer chain on current level i
           // Then go down one level on the last reached node of the chain
           for-loop(while-loop(x, i), i - 1)
          end;
        end for-loop,
        
        method while-loop (x, i :: <integer>)
          // Follow pointer chain on level i
          // while the end is not reached
          //       and key < skey
          if (x.next[i] & key<(x.next[i].key, skey))
            while-loop(x.next[i], i);
          else
            // Return last reached node
            x
          end;
        end while-loop;
        
  let x = for-loop(sl, sl.level - 1);
  */

  if (~x | ~key==(x.key, skey))
    if (unsupplied?(default))
      error("Key %= not in %=", skey, sl);
    else
      default
    end
  else 
    x.value
  end
end method element;


define method element-setter (val, sl :: <basic-skip-list>, nkey)
  => val :: <object>;
  // Insert the new node in key order, and at the end of the list as iterated.
  
  // Remember which pointers must be changed to point to the new node
  let update = sl.skip-list-update-cache;
  
  let x = find-key-and-maintain-update(sl,nkey);
  
  if (x & sl.key-test(x.key, nkey))
    // It was already there, just change the value
    x.value := val;
  else
    // create a new node
    let nlevel = random-level(sl);
    let nnode = make(<skip-list-node>, level: nlevel, key: nkey, value: val);
    // Remember the old-level for the update task
    let old-level = sl.level;
    
    // May need to expand the next vector
    if (nlevel > sl.next.size)
      let new-next :: <next-node-vector> = make(<next-node-vector>,
                                              size: nlevel);
      let old-next :: <next-node-vector> = sl.next;
      
      // Copy the old references
      for (n from 0 below old-level)
        new-next[n] := old-next[n];
      end for;
      // Set up new references which must point to the new node
      for (n from old-level below nlevel)
        new-next[n] := nnode;
      end for;
      sl.next := new-next;
    end if;
    
    // Update the pointers, only for the ones which can actually point 
    // onto the new node
    for (i from 0 below min(old-level, nlevel))
      let onode = update[i];
      nnode.next[i] := onode.next[i];
      onode.next[i] := nnode;
    end;
    
    // Update the iteration pointers
    if (sl.private-size = 0)
      sl.forward := nnode;
      sl.backward := nnode;
    else
      let onode = sl.backward;
      onode.forward := nnode;
      sl.backward := nnode;
    end;

    // increment the size of this collection
    sl.private-size := sl.private-size + 1;
    
    // update current maximum level used
    sl.level := max(sl.level, nlevel);
    
    // create a new update cache if necessary
    if (sl.next.size > update.size)
      sl.skip-list-update-cache := make(<next-node-vector>,
                                        size: sl.next.size + 8,
                                        fill: #f);
    end;
  end if;
  val;
end method;


define method remove-key! (sl :: <basic-skip-list>, skey)
  => removed? :: <boolean>;
  // Delete the node containing key
  // Return #t if key was in the collection and deleted
  // Return #f if key was not in the collection and therefore not deleted
  
  // Remember which pointers must be changed to point to the new node
  let update = sl.skip-list-update-cache;
    
  let x = find-key-and-maintain-update(sl,skey);
  
  if (x & sl.key-test(x.key, skey))
    // we found it, delete it
    for (i from 0 below x.next.size)
      // savely ignore all fields above
      update[i].next[i] := x.next[i];
    end;

    // decrement the size of this collection
    sl.private-size := sl.private-size - 1;

    // Decrement level, search down the header for first non-empty reference
    for (i from sl.level - 1 to 0 by -1 ,
         until: sl.next[i])
      finally sl.level := i + 1;
    end;
    
    // Update forward and backward iteration pointers
    case
      x.backward => x.backward.forward := x.forward;
      x.forward => x.forward.backward := x.backward;
    end case;
    if (sl.forward == x)
      sl.forward := x.forward;
    end if;
    if (sl.backward == x)
      sl.backward := x.backward;
    end if;

    #t
  else
    #f
  end;
end method;


//==============================================================================


// A skip-list-node contains the actual data of the skip lists
//
define sealed primary class <skip-list-node> (<object>)
  // We can seal it safely, because the library user should 
  // not know this class exists.  The search key must be comparable by the 
  // key-order function specified for the skip-list
  slot key :: <object>, init-value: #f,
                        init-keyword: key:;
  slot value :: <object>, init-value: #f,
                          init-keyword: value:;

  slot next :: <next-node-vector>, init-value: make(<next-node-vector>), 
                                   init-keyword: next:;

  slot private-forward :: false-or(<skip-list-node>),
     init-value: #f,
     init-keyword: forward:;
  slot private-backward :: false-or(<skip-list-node>),
     init-value: #f,
     init-keyword: backward:;
end class;


define method initialize (node :: <skip-list-node>,
                          #key level: nlevel);
  next-method();
  node.next := make(<next-node-vector>,
                    size: nlevel,
                    fill: #f);
end method;


define inline method forward (node :: <skip-list-node>)
  => (fwd :: false-or(<skip-list-node>))
  node.private-forward;
end method;


define method forward-setter
     (ptr :: false-or(<skip-list-node>), node :: <skip-list-node>)
  => (ptr)
  node.private-forward := ptr;
  if (ptr)
    ptr.private-backward := node;
  end if;
end method;


define inline method backward (node :: <skip-list-node>)
  => (back :: false-or(<skip-list-node>))
  node.private-backward;
end method;


define method backward-setter
     (ptr :: false-or(<skip-list-node>), node :: <skip-list-node>)
  => (ptr)
  node.private-backward := ptr;
  if (ptr)
    ptr.private-forward := node;
  end if;
end method;


//==============================================================================
//
// Protocols
//
//==============================================================================


define inline method size (skip :: <basic-skip-list>) => (size :: <integer>)
  skip.private-size
end method;


// as allows you to convert any collection with keys 
// comparable by < into a skip-list
//
define method as(new-type == <basic-skip-list>, coll :: <collection>)
             => (result :: <basic-skip-list>);
  // Make a skip-list from the collection coll
  
  copy-key-value-pairs(coll, make(new-type));
end method as;


define method as(new-type == <skip-list>, coll :: <collection>)
             => (result :: <skip-list>);
  
  let sl = copy-key-value-pairs(coll, make(new-type));
  // clean up after converting
  clear-cache(sl);
  sl;
end method as;


define method copy-key-value-pairs(from :: <collection>, into :: <collection>)
                               => (result :: <collection>);
  // copy all key/value pairs from 'from' into 'into'
  let (initial-state,
       limit,
       next-state,
       finished-state?,
       current-key,
       current-element) = forward-iteration-protocol(from);
  
  for (state = initial-state then next-state(from, state),
       until: finished-state?(from, state, limit))
    into[current-key(from, state)] := current-element(from, state);
  end for;
  into;
end method;


//==============================================================================
//
// Iteration
//
//==============================================================================


// DJV 07.03.2009  changed iteration protocol
// The iteration just goes through the forward or backward links of the list
// The iteration state is encoded by the current skip-list-node


define method forward-iteration-protocol (skip-list :: <basic-skip-list>)
  => (initial-state :: <object>,
      limit :: <object>,
      next-state :: <function>,
      finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>,
      current-element-setter :: <function>,
      copy-state :: <function>);
  values(skip-list.forward,   // The first element of the skip-list
         #f,                  // The end of the list
         skip-list-fip-next-state, 
         skip-list-ip-finished-state?,
         skip-list-ip-current-key,
         skip-list-ip-current-element,
         skip-list-ip-current-element-setter,
         skip-list-ip-copy-state);
end method forward-iteration-protocol;


define method backward-iteration-protocol (skip-list :: <basic-skip-list>)
  => (initial-state :: <object>,
      limit :: <object>,
      next-state :: <function>,
      finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>,
      current-element-setter :: <function>,
      copy-state :: <function>);
  values(skip-list.backward,  // The first element of the skip-list
         #f,                  // The end of the list
         skip-list-bip-next-state, 
         skip-list-ip-finished-state?,
         skip-list-ip-current-key,
         skip-list-ip-current-element,
         skip-list-ip-current-element-setter,
         skip-list-ip-copy-state);
end method backward-iteration-protocol;


// This iterates in key order, as opposed to the natural element order. If
// you do not want element order, you can import this module, exclude
// forward- and backward-iteration-protocol, and define a local fip that calls
// this method.
//
// This method just iterates through next[0].
//
define method forward-by-key-iteration-protocol (skip-list :: <basic-skip-list>)
  => (initial-state :: <object>,
      limit :: <object>,
      next-state :: <function>,
      finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>,
      current-element-setter :: <function>,
      copy-state :: <function>);
  values(skip-list.next[0],   // The first element of the skip-list
         #f,                  // The end of the list
         skip-list-kip-next-state, 
         skip-list-ip-finished-state?,
         skip-list-ip-current-key,
         skip-list-ip-current-element,
         skip-list-ip-current-element-setter,
         skip-list-ip-copy-state);
end method forward-by-key-iteration-protocol;


define method skip-list-fip-next-state
     (list :: <basic-skip-list>, state :: <skip-list-node>)
  => (result :: false-or(<skip-list-node>));
  // Just get the immediate successor
  state.forward;
end method;


define method skip-list-bip-next-state
     (list :: <basic-skip-list>, state :: <skip-list-node>)
  => (result :: false-or(<skip-list-node>));
  // Just get the immediate predecessor
  state.backward;
end method;


define method skip-list-kip-next-state
     (list :: <basic-skip-list>, state :: <skip-list-node>)
  => (result :: false-or(<skip-list-node>));
  // Just get the next node in key order.
  state.next[0];
end method;


define method skip-list-ip-finished-state?
     (list :: <basic-skip-list>, state :: false-or(<skip-list-node>), limit)
  state == limit;
end method;


define method skip-list-ip-current-key
     (list :: <basic-skip-list>, state :: <skip-list-node>)
  => (result :: <object>);
  state.key;
end method;


define method skip-list-ip-current-element
     (list :: <basic-skip-list>, state :: <skip-list-node>)
  => (result :: <object>);
  state.value;
end method;


define method skip-list-ip-current-element-setter
     (value :: <object>, list :: <basic-skip-list>, state :: <skip-list-node>)
  => (result :: <object>);
  state.value := value;
end method;


define method skip-list-ip-copy-state
     (list :: <basic-skip-list>, state :: <skip-list-node>)
  => (result :: <skip-list-node>);
  state;
end method;


//==============================================================================
//
// Direct element manipulation
//
// 06.03.2009  DJV
//
//==============================================================================


// One of the useful features of skip lists is that they can be ordered.
// However, most of the useful operations that can be performed on ordered
// collections, such as sort, are only defined for sequences. To solve this
// problem, I add element-sequence and element-sequence-setter. The client may
// call the former to obtain a sequence, operate on it, and call the latter to
// fix the results in the skip list. The setter must ensure that every element
// has a key and that every key has an element.


define method element-sequence
     (list :: <basic-skip-list>)
  => (result :: <sequence>)
  as(<simple-object-vector>, list)
end method;


// This method sets the order of iteration. If the elements of the new element
// sequence are not identical to the elements of the skip list, an error is
// signaled.
//
define method element-sequence-setter
     (elems :: <sequence>, list :: <basic-skip-list>)
  => (elems :: <sequence>)
  if (elems.size ~= list.size)
    error("Skip list and element sequence contain different elements");
  end if;
  for (target in elems,
       cur-node = list.forward then cur-node.forward)
    let found-node = find-node-value(cur-node, target);
    if (found-node)
      swap-nodes(list, cur-node, found-node);
      cur-node := found-node;
    else
      error("Skip list does not contain element %= in element sequence",
            target);
    end if;
  end for;
  elems;
end method;


define method swap-nodes
     (sl :: <skip-list>, node1 :: <skip-list-node>, node2 :: <skip-list-node>)
  => ()
  if (node1 ~== node2)
    let temp-forward = node1.forward;
    node1.forward := node2.forward;
    node2.forward := temp-forward;

    let temp-backward = node1.backward;
    node1.backward := node2.backward;
    node2.backward := temp-backward;
  
    select (sl.forward)
      node1 => sl.forward := node2;
      node2 => sl.forward := node1;
      otherwise => #f;
    end select;
  
    select (sl.backward)
      node1 => sl.backward := node2;
      node2 => sl.backward := node1;
      otherwise => #f;
    end select;
  end if;
end method;


define method find-node-value
     (start :: false-or(<skip-list-node>), val)
  => (node :: false-or(<skip-list-node>))
  for (node = start then node.forward,
       while: node & node.value ~== val)
  finally
    node
  end for;
end method;


//==============================================================================
//
// Printing
//
// 07.03.2009  DJV
//
//==============================================================================


define method print-object (o :: <basic-skip-list>, s :: <stream>) => ()
  printing-logical-block (s, prefix: "{", suffix: "}")
    write(s, "skip-list ");
    for (e keyed-by k in o)
      pprint-newline(#"fill", s);
      format(s, "%= ", e);
      pprint-newline(#"fill", s);
      format(s, "keyed-by %=, ", k);
    end for;  
  end printing-logical-block;
end method;


//==============================================================================
//
// The class for use by most library users
//
//==============================================================================


define open primary class <skip-list> (<basic-skip-list>)
  // A skip-list that always clears its internal cache 
  // after modifying the data structure keys
end class;


define method element-setter (val, sl :: <skip-list>, nkey)
  => val :: <object>;
  // Insert the new node into the list then clear cache
  let result = next-method();
  clear-cache(sl);
  result;
end method;


define method remove-key! (sl :: <skip-list>, skey)
  => removed? :: <boolean>;
  // Delete the node containing key, then clean up
  // Return #t if key was in the collection and deleted
  // Return #f if key was not in the collection and therefore not deleted
  let result = next-method();
  clear-cache(sl);
  result;
end method;

// End of File
