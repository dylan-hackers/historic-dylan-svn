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

// Return the count of the number of items in the sequence where
// PREDICATE(item) returns true.
define method count-if(sequence :: <sequence>, predicate :: <function>) => (r :: <integer>)
  let count :: <integer> = 0;
  for(item in sequence)
    when(predicate(item))
      count := count + 1;
    end when;
  end for;
  count
end method count-if;

// Return the count of the number of items in the sequence where
// PREDICATE(item) returns false.
define method count-if-not(sequence :: <sequence>, predicate :: <function>) => (r :: <integer>)
  let count :: <integer> = 0;
  for(item in sequence)
    unless(predicate(item))
      count := count + 1;
    end unless;
  end for;
  count
end method count-if-not;

// Return the indexes of a substring bounded by START-SEQUENCE and
// END-SEQUENCE in the string IN-SEQUENCE.
define method find-between(in-sequence :: <string>,
                           start-sequence :: <string>,
                           end-sequence :: <string>,
                           #key start = 0)
 => (start-index, end-index)
  let in-sequence = copy-sequence(in-sequence, start: start);
  let found-start = subsequence-position(in-sequence, start-sequence);
  when(found-start)
    let found-start = found-start + start-sequence.size;
    let the-end = subsequence-position(copy-sequence(in-sequence, start: found-start), end-sequence);
    when(the-end)
      values(found-start + start, the-end + found-start + start);
    end when;
  end when;
end method find-between;

// Search for a substring in another string
define method search(in-sequence :: <string>, 
                     pattern :: <string>, 
                     #key start: the-start :: <integer> = 0,
                          end: the-end :: <integer> = in-sequence.size)
 => (r :: false-or(<integer>))
  let find-in = copy-sequence(in-sequence, start: the-start, end: the-end);
  let found = subsequence-position(find-in, pattern);
  found & found + the-start;
end method search;

// Convert a string to all uppercase
define method string-upcase(s :: <string>) => (s :: <string>)
  local method lower-case?(ch :: <character>) => (r :: <boolean>)
    ch >= 'a' & ch <= 'z';
  end method;

  let result = make(<string>, size: s.size);
  for(ch keyed-by index in s)
    result[index] :=
      if(lower-case?(ch))
        as(<character>, 
          as(<integer>, ch) + 
          as(<integer>, 'A') -
          as(<integer>, 'a'))
      else
        ch;
      end if;
  end for;
  result;
end method string-upcase;

// Convert a string to all lowercase
define method string-downcase(s :: <string>) => (s :: <string>)
  local method upper-case?(ch :: <character>) => (r :: <boolean>)
    ch >= 'A' & ch <= 'Z';
  end method;

  let result = make(<string>, size: s.size);
  for(ch keyed-by index in s)
    result[index] :=
      if(upper-case?(ch))
        as(<character>, 
          as(<integer>, ch) + 
          as(<integer>, 'a') -
          as(<integer>, 'A'))
      else
        ch;
      end if;
  end for;
  result;
end method string-downcase;


