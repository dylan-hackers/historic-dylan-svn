Module:    base64
Synopsis:  Base64 encoding/decoding routines
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

// A class that represents base64 encoded data. This class is the result
// of base64 encoding data and can be decoded to various types such as
// <string>, <vector>, etc.
define class <base64> (<object>)
  // The string containing the base64 encoded representation of the data.
  constant slot base64-string :: <string>, required-init-keyword: string:;
end class <base64>;

// The allowed types that a <base64> object can be converted to.
define constant <base64-byte> = limited(<integer>, min: 0, max: 255);
define constant <base64-vector> = limited(<vector>, of: <base64-byte>);

// Encodes the given sequence as a <base64 > object.
define generic base64-encode( o :: <sequence> ) => (r :: <base64>);

// Decodes the <base64> object, returning it as a object of the specified type.
define generic base64-decode-as ( type :: <class>, b :: <base64> ) => (t :: <object>);

// Base64 Encoding/Decoding methods belows translated 
// from Common Lisp versions in the AllegroServe
// Open Source project.
//
// From the AllegroServe source, a description of the
// algorithm:
/*
;;;; encoding algorithm:
;; each character is an 8 bit value.
;; three 8 bit values (24 bits) are turned into four 6-bit values (0-63)
;; which are then encoded as characters using the following mapping.
;; Zero values are added to the end of the string in order to get
;; a size divisible by 3 (these 0 values are represented by the = character
;; so that the resulting characters will be discarded on decode)
;; 
;; encoding
;; 0-25   A-Z
;; 26-51  a-z
;; 52-61  0-9
;; 62     +
;; 63     /
*/

define variable *base64-decode* =
  begin
    let array = make(<vector>, size: 128, fill: 0);
    for(i from 0 by 1,
      ch = as(<integer>, 'A') then ch + 1,
      until: ch > as(<integer>, 'Z'))
        array[ch] := i;
    end for;
    for(i from 26 by 1,
      ch = as(<integer>, 'a') then ch + 1,
      until: ch > as(<integer>, 'z'))
        array[ch] := i;
    end for;
    for(i from 52 by 1,
      ch = as(<integer>, '0') then ch + 1,
      until: ch > as(<integer>, '9'))
        array[ch] := i;
    end for;
    array[as(<integer>, '+')] := 62;
    array[as(<integer>, '/')] := 63;
    values(array);
  end;
    
define variable *base64-encode* =
    begin
      let array = make(<vector>, size: 64);
      for(i from 0 below 26)
        array[i] := as(<character>, as(<integer>, 'A') + i);
      end for;
      for(i from 0 below 26)
        array[i + 26] := as(<character>, as(<integer>, 'a') + i);
      end for;
      for(i from 0 below 10)
        array[i + 52] := as(<character>, as(<integer>, '0') + i);
      end for;
      array[62] := '+';
      array[63] := '/';
      values(array);
    end;

define method base64-decode-as( type :: <class>, b64 :: <base64>) => (result :: <object>)
  let string = b64.base64-string;
  let result = make(<stretchy-vector>);
  let array = *base64-decode*;
  for(i from 0 by 4,
      until: i >= string.size)
    let val = ash(array[as(<integer>, string[i])], 18) +
              ash(array[as(<integer>, string[i + 1])], 12) +
              ash(array[as(<integer>, string[i + 2])], 6) +
              array[as(<integer>, string[i + 3])];
    let cha = string[i + 2];
    let chb = string[i + 3];
    result := add!(result, as(<character>, ash(val, -16)));
    if(cha ~= '=')
      result := add!(result, as(<character>, logand(#xff, ash(val, -8))));
    end if;
    if(chb ~= '=')
      result := add!(result, as(<character>, logand(#xff, val)));
    end if;
  end for;
  select(type)
    <base64-vector> => map-as(<base64-vector>, curry(as, <integer>), result);
    <string> => as(<byte-string>, result);
    otherwise => error("Cannot base64-decode to the given type.");
  end select;
end method base64-decode-as;

define method base64-encode( string :: <sequence> ) => (result :: <base64>)
  let result = make(<stretchy-vector>);
  let v1 = 0;
  let v2 = 0;
  let v3 = 0;
  let eol = #f;
  let from = 0;
  let max = string.size;
  block(return)
    while(#t)
      if(from >= max)
        return()
      end if;
      v1 := as(<integer>, string[from]);     
      from := from + 1;
      if(from >= max)
        v2 := 0;
        eol := #t;
      else
        v2 := as(<integer>, string[from]);
      end if;
      
      from := from + 1;
      result := add!(result, *base64-encode*[logand( #x3f, ash( v1, -2 ) )]);
      result := add!(result, *base64-encode*[ash(logand( 3, v1 ), 4) + logand(#xf, ash( v2, -4))]);
      
      if(eol)
        result := add!(result, '=');
        result := add!(result, '=');
        return();
      end if;
      
      if(from >= max)
        v3 := 0;
        eol := #t;
      else
        v3 := as(<integer>, string[from]);
      end if;
      
      from := from + 1;
      
      result := add!(result, *base64-encode*[ash(logand(#xf, v2), 2) + logand(3, ash(v3, -6))]);
      if(eol)
        result := add!(result, '=');
        return();
      end if;
      
      result := add!(result, *base64-encode*[logand(#x3f, v3)]);
    end while;
  end block;
  make(<base64>, string: as(<byte-string>, result)); 
end method base64-encode;

