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

// A <form-query-item> represents the name=data part
// of a query.
define class <form-query-item> (<object>)
  slot form-query-item-key :: <string>, init-keyword: key:;
  slot form-query-item-value :: <string>, init-keyword: value:;
end class <form-query-item>;

// A form query, containing a sequence of <form-query-item>'s.
define class <form-query> (<object>)
  slot form-query-items :: <vector>;
end class <form-query>;

// Given a sequence of (key, value, key, value, ...)
// return a form query.
define method initialize(query :: <form-query>, #key items)
  next-method();

  if(items)
    query.form-query-items := make(<vector>, size: truncate/(items.size, 2));
    for(index from 0 below query.form-query-items.size)
      query.form-query-items[index] := make(<form-query-item>,
                                            key: items[index * 2],
                                            value: items[index * 2 + 1]);
    end for;  
  else
    query.form-query-items := make(<stretchy-vector>);
  end if;
end method initialize;


define method form-query-encode( query :: <form-query> )
 => (r :: <string>)
  let result = "";
  for(item in query.form-query-items)
    unless(empty?(result))
      result := concatenate(result, "&");
    end unless;

    result := concatenate(result, 
                          form-url-encode(item.form-query-item-key),
                          "=",
                          form-url-encode(item.form-query-item-value));
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
 
  make(<form-query>, items: items);
end method form-query-decode;

