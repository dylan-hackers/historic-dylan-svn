Module:    base64
Synopsis:  Base64 encoding/decoding
Author:    Carl Gay
License:   This code is in the public domain
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// This file implements the Base64 transfer encoding algorithm as
// defined in RFC 1521 by Borensten & Freed, September 1993.
//
// Original version written in Common Lisp by Juri Pakaste <juri@iki.fi>.
// Converted to Dylan by Carl Gay, July 2002.

define constant $standard-encoding-vector :: <byte-string>
  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

define constant $http-encoding-vector :: <byte-string>
  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$!@";

// ---TODO: line breaks?
//define constant $base64-line-break :: <byte-string> = "\n";

// I thought FunDev had <integer-vector> built in, but apparently not.
//
define constant <int-vector> = limited(<vector>, of: <integer>);

define function make-decoding-vector
    (encoding-vector) => (v :: <int-vector>)
  let v = make(<int-vector>, size: 256, fill: -1);
  for (index from 0 below v.size,
       char in encoding-vector)
    v[as(<integer>, char)] := index;
  end;
  v
end;

define constant $standard-decoding-vector :: <int-vector>
  = make-decoding-vector($standard-encoding-vector);

define constant $http-decoding-vector :: <int-vector>
  = make-decoding-vector($http-encoding-vector);

define function base64-encode
    (string :: <byte-string>, #key encoding :: <symbol> = #"standard")
 => (s :: <byte-string>)
  let encoding-vector :: <byte-string>
    = select (encoding)
        #"standard" => $standard-encoding-vector;
        #"http"     => $http-encoding-vector;
      end;
  let result = make(<byte-string>, size: 4 * floor/(2 + string.size, 3));
  for (sidx from 0 by 3,
       didx from 0 by 4,
       while: sidx < string.size)
    let chars = 2;
    let value = ash(logand(#xFF, as(<integer>, string[sidx])), 8);
    for (n from 1 to 2)
      when (sidx + n < string.size)
        let char-code :: <integer> = as(<integer>, string[sidx + n]);
        value := logior(value, logand(#xFF, char-code));
        inc!(chars);
      end;
      when (n = 1)
        value := ash(value, 8);
      end;
    end;
    result[didx + 3] := encoding-vector[iff(chars > 3, logand(value, #x3F), 64)];
    value := ash(value, -6);
    result[didx + 2] := encoding-vector[iff(chars > 2, logand(value, #x3F), 64)];
    value := ash(value, -6);
    result[didx + 1] := encoding-vector[logand(value, #x3F)];
    value := ash(value, -6);
    result[didx + 0] := encoding-vector[logand(value, #x3F)];
  end;
  result
end;
    
define function base64-decode
    (string :: <byte-string>, #key encoding :: <symbol> = #"standard")
 => (s :: <byte-string>)
  let result = make(<byte-string>, size: 3 * floor/(string.size, 4));
  let ridx :: <integer> = 0;
  block (exit-block)
    let decoding-vector :: <int-vector>
      = select (encoding)
          #"standard" => $standard-decoding-vector;
          #"http"     => $http-decoding-vector;
        end;
    let bitstore :: <integer> = 0;
    let bitcount :: <integer> = 0;
    for (char :: <byte-character> in string)
      let value = decoding-vector[as(<integer>, char)];
      unless (value == -1 | value == 64)
        bitstore := logior(ash(bitstore, 6), value);
        inc!(bitcount, 6);
        when (bitcount >= 8)
          dec!(bitcount, 8);
          let code = logand(ash(bitstore, 0 - bitcount), #xFF);
          if (zero?(code))
            exit-block();
          else
            result[ridx] := as(<byte-character>, code);
            inc!(ridx);
            bitstore := logand(bitstore, #xFF);
          end;
        end;
      end;
    end;
  end block;
  copy-sequence(result, start: 0, end: ridx)
end;

