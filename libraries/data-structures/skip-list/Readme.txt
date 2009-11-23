Copyright (c) 1996 by Kai W. Zimmermann, Hamburg, Germany
All rights reserved
GNU Lesser General Public License (LGPL)

Version   1.0
Released  17.03.1996

Kai W. Zimmermann, Hamburg, Germany
kwz@kai-zimmermann.de


Description
===========
 
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
--
kwz@kai-zimmermann.de
