Module:    encode-decode
Synopsis:  Routines for encoding and decoding URI's
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

// The following encoding and decoding functions are based
// on code from AllegroServe, a Common Lisp web server.

// *uri-encode* is an array keyed by the value of
// all 7 bit character codes converted to an <integer>.
// Those characters that need to be encoded have their
// element in the array set to #t.
define variable *uri-encode* =
  begin
    let result = make(<vector>, size: 128, fill: #t);
    
    // Start with uppercase letters
    for(index from as(<integer>, 'A') to as(<integer>, 'Z'))
      result[index] := #f;
    end for;

    // Now lowercase letters
    for(index from as(<integer>, 'a') to as(<integer>, 'z'))
      result[index] := #f;
    end for;

    // Numbers
    for(index from as(<integer>, '0') to as(<integer>, '9'))
      result[index] := #f;
    end for;

    // Miscellaneous other characters
    for(ch in #('-', '_', '.', '!', '~', '*', '\'', '(', ')'))
      result[as(<integer>, ch)] := #f;
    end for;

    result;
  end;

// Return #t if the character must be encoded to %xy in a URI
define function uri-encode?(ch :: <character>)
 => (r :: <boolean>)
  let code :: <integer> = as(<integer>, ch);
  code >= 128 | *uri-encode*[code]; 
end function uri-encode?;

// Array of hexadecimal digits for conversions
define constant *hex-digits* = #[ '0', '1', '2', '3',
                                  '4', '5', '6', '7',
                                  '8', '9', 'a', 'b',
                                  'c', 'd', 'e', 'f' ];

// Convert a hexadecimal character into a digit.
define function hex-digit-to-integer(ch :: <character>)
 => (r :: <integer>)
  case
   (ch >= '0' & ch <= '9') => as(<integer>, ch) - as(<integer>, '0');
   (ch >= 'a' & ch <= 'f') => as(<integer>, ch) - as(<integer>, 'a') + 10;
   (ch >= 'A' & ch <= 'F') => as(<integer>, ch) - as(<integer>, 'A') + 10;
   otherwise => error("Invalid hex digit passed to hex-digit-to-integer");
  end case;  
end function hex-digit-to-integer;

// Encode a string using URI encoding
define method uri-encode( s :: <string> )
 => (r :: <string>)
  let result = make(<stretchy-vector>);
  for(ch :: <character> in s)
    if(uri-encode?(ch))
      let code = as(<integer>, ch);
      let up-code = logand(#xf, ash(code, -4));
      let down-code = logand(#xf, code);
      result := add!(result, '%');
      result := add!(result, *hex-digits*[up-code]);
      result := add!(result, *hex-digits*[down-code]);
    else
      result := add!(result, ch);
    end if;
  end for;
  as(<string>, result);
end method uri-encode;

// Convert a string with %xx hex escapes into a string without.
// If space? is #t then also convert +'s into spaces.
define function un-hex-escape(s :: <string>, #key space?)
  let result = make(<stretchy-vector>);
  for(index from 0 below s.size)
    if(s[index] = '%')
      let new-ch = as(<character>, 
                      ash(hex-digit-to-integer(s[index + 1]), 4) + 
                      hex-digit-to-integer(s[index + 2]));
      if(new-ch = '\n' & ~empty?(result) & result.last = 'r')
        result[result.size - 1] := new-ch;
      else
        result := add!(result, new-ch);
      end if;
      index := index + 2;
    elseif(space? & s[index] = '+')
      result := add!(result, ' ');
    else
      result := add!(result, s[index]);
    end if;
  end for;
  as(<string>, result);
end function un-hex-escape;

define method uri-decode(s :: <string>) 
 => (r :: <string>)
  un-hex-escape(s);
end method uri-decode;

// Maps 7 bit character codes to #f if they have to be encoded.
// The element in the table is #f if no encoding needed or
// an integer representing how many extra characters are needed
// to encode this.
define variable *url-form-encode* =
  begin
    let result = make(<vector>, size: 128, fill: #f);
    for(index from 0 below 32)
      result[index] := 2;
    end for;

    result[127] := 2;

    for(ch in #['+', '#', ';', '/', '?', ':', '@', '=', '&', '<', '>', '%'])
      result[as(<integer>, ch)] := 2;
    end for;

    result[as(<integer>, ' ')] := 0;
    result[as(<integer>, '\n')] := 5;

    result;
  end;

// Encode the string as a form-url encoded string.
define method form-url-encode(s :: <string>)
 => (r :: <string>)
  let result = make(<stretchy-vector>);
  for(ch in s)
    let code = as(<integer>, ch);
    if(ch = ' ')
      result := add!(result, '+');
    elseif(ch = '\n')
      result := add!(result, '%');
      result := add!(result, '0');
      result := add!(result, 'd');
      result := add!(result, '%');
      result := add!(result, '0');
      result := add!(result, 'a');
    elseif(code >= 128 | *url-form-encode*[code])
      result := add!(result, '%');
      result := add!(result, *hex-digits*[logand(#xf, ash(code, -4))]);
      result := add!(result, *hex-digits*[logand(#xf, code)]);
    else
      result := add!(result, ch);
    end if;
  end for;
  as(<string>, result);
end method form-url-encode;

define method form-url-decode( s :: <string> )
 => (r :: <string>)
  un-hex-escape(s, space?: #t);
end method form-url-decode;

// Forms queries are key/value pairs. Fits nicely with
// a <string-table>. Maybe the keys could be symbols and make
// it a normal table but that would prevent case sensitive keys.
define constant <form-query> = <table>;

// Given a sequence of (key, value, key, value, ...)
// return a form query.
define method make-form-query(items :: <sequence>)
 => (query :: <form-query>)
   let query = make(<string-table>);
   let items-size = truncate/(items.size, 2);
   for(index from 0 below items-size)
     query[items[index * 2]] := items[index * 2 + 1];
   end for;  
   query;
end method make-form-query;


define method form-query-encode( query :: <form-query> )
 => (r :: <string>)
  let result = "";
  for(value keyed-by key in query)
    unless(empty?(result))
      result := concatenate(result, "&");
    end unless;

    result := concatenate(result, 
                          form-url-encode(key),
                          "=",
                          form-url-encode(value));
  end for;
  result;
end method form-query-encode;

define method form-query-decode( s :: <string> )
 => (r :: <form-query>)
  let items = make(<stretchy-vector>);
  let start-index = 0;
  let state = #"key";
  for(ch keyed-by current-index in s)
    select(state)
      #"key"
        => begin
             when(ch = '=')
               state := #"switch-to-value";                             
               items := add!(items, 
                             form-url-decode(copy-sequence(s, 
                                                           start: start-index, 
                                                           end: current-index)));
             end when;
           end;
      #"value" 
        => begin
             when(ch = '&')
               state := #"switch-to-key";
               items := add!(items, 
                             form-url-decode(copy-sequence(s,
                                                           start: start-index,
                                                           end: current-index)));
             end when;
           end;
      #"switch-to-value"
       => begin
            state := #"value";
            start-index := current-index;
          end;
      #"switch-to-key"
       => begin
            state := #"key";
            start-index := current-index;
          end;
      otherwise
       => error("Invalid state in form-query-decode state machine.");
    end select;
  finally
    items := add!(items, form-url-decode(copy-sequence(s,
                                                       start: start-index)));
  end for;
 
  make-form-query(items);
end method form-query-decode;

