Module:    sequence-utilities
Synopsis:  Various utility functions and classes related to sequences
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define method sequence-position
    (sequence, item, 
    #key start: the-start = 0, end: the-end = size(sequence), from-end?, key, test, test-not)
 => (index :: false-or(<integer>))
 block(return)
   let (the-start, the-end, increment) =
     if(from-end?)
       values(the-end - 1, the-start - 1, -1)
     else
       values(the-start, the-end, 1)
     end if;
   
   let test = test | 
     if(test-not)
       complement(test-not)
     else
       \=
     end if;
   
   
   let key = key | identity;
   
   for(i = the-start then i + increment, until: i = the-end)
     when(test(key(sequence[i]), item))
       return(i)
     end when;
   end for;
   #f;
 end block;
end method sequence-position;

// Implementation adapted from primitive-position-if in Deuce source code with
// addition of KEY.
define method sequence-position-if
    (sequence, predicate, 
    #key start: the-start = 0, end: the-end = size(sequence), from-end?, key)
 => (index :: false-or(<integer>))
  block(return)
    let (the-start, the-end, increment) =
      if(from-end?)
        values(the-end - 1, the-start - 1, -1)
      else
        values(the-start, the-end, 1)
      end if;

    let key = key | identity;

    for(i = the-start then i + increment, until: i = the-end)
      when(predicate(key(sequence[i])))
        return(i)
      end when;
    end for;
    #f;
  end block;
end method sequence-position-if;

define method sequence-position-if-not
    (sequence, predicate, 
    #key start: the-start = 0, end: the-end = size(sequence), from-end?, key)
 => (index :: false-or(<integer>))
  sequence-position-if(sequence, complement(predicate), start: the-start, end: the-end, from-end?: from-end?, key: key);
end method sequence-position-if-not;

// Tests
// sequence-position-if(#(#(1), #(2), #(3), #(4)), odd?, start: 1, key: first);
//   => 2
//
// sequence-position-if-not(#(1, 2, 3, 4, 5.0), rcurry(instance?, <integer>));
//   => 4
//
// sequence-position("baobab",'a', from-end?: #t);
//   => 4
//
// sequence-position(#(), 595);
//   => #f
//
// sequence-position(#(#(1), #(2), #(3), #(4)), 3, start: 1, key: first);
//   => 2

// Adapted from Common Lisp version in CLOCC
define method split-sequence
    (sequence, predicate, 
    #key start: the-start = 0, end: the-end = size(sequence), key, strict?)
  let st1 = 0;
  let result = make(<stretchy-vector>);
  for(st0 = if(strict?) 
              the-start 
            else 
              sequence-position-if-not(sequence, predicate, start: the-start, end: the-end, key: key)
            end if
          then
            if(strict?)
              if(st1)
                st1 + 1
              else
                #f
              end if;
            else
              sequence-position-if-not(sequence, predicate, start: st1 | st0, end: the-end, key: key)
            end if,
      while: st0 & st1)
    st1 := sequence-position-if(sequence, predicate, start: st0, end: the-end, key: key);
    result := add!(result, copy-sequence(sequence, start: st0, end: st1 | the-end));
  end for;
  result;
end method split-sequence;

define method split-string(string, chars, #rest options)
  apply(split-sequence, string, method(x) member?(x, chars) end, options);
end method split-string;

