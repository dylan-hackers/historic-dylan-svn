Module:    xml-rpc-common
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

/*
/// Dylan implementation of Internet Base64 encoding.
/// See RFC-1113 and RFC-1341.

/// Copyright Massachusetts Institute of Technology, 1994
/// Written by Mark Nahabedian

/// Converted for HotDispatch 23-may-00, gz@hotdispatch.com.
/// Converted to Dylan 20-jul-2002 by Carl Gay


define function base64-encode-string
    (string :: <byte-string>, #key buffer, max-line-length = 64, standard-encoding?)
 => (encoded-string :: <byte-string>)
  local method determine-string-length (n-bytes :: <integer>)
          let (lines :: <integer>, chars :: <integer>) = floor/(n-bytes, max-line-length);
          n-bytes
            + iff(zero?(lines),
                  0,
                  size($base64-line-break)
                  * iff(zero?(chars), lines - 1, lines))
        end;
  let start :: <integer> = 0;
  let _end :: <integer> = string.size;
  let n-bytes :: <integer> = _end - start;
  let n-bytes-encoded :: <integer> = 4 * ceiling/(8 * n-bytes, 24);
  let length :: <integer> = determine-string-length(n-bytes-encoded);
  let result :: <byte-string> = buffer | make(<byte-string>, size: length);
  let in :: <integer> = start;
  let out :: <integer> = 0;
  let out-this-line :: <integer> = 0;
  let encoding-vector = iff(standard-encoding?,
                            $standard-encoding-vector,
                            $http-encoding-vector);
  let fill-char :: <byte-character> = encoding-vector[64];
  when (buffer)
    assert(length > buffer.size,
           "buffer too small to hold base64 encoding");
  end;
  local
    method getbyte () => (byte :: <integer>)
      let x = in;
      inc!(in);
      as(<integer>, string[x]);
    end,
    // 111111 112222 222233 333333
    // 111111 222222 333333 444444
    method first12 (byte1 :: <integer>, byte2 :: <integer>)
      logior(ash(byte1, 4), ash(byte2, -4));
    end,
    method second12 (byte2 :: <integer>, byte3 :: <integer>)
      // low 4 bits of byte2 and all 8 bits of byte3
      logior(ash(logand(byte2, #b1111), 8), byte3);
    end,
    method putbyte (byte)
      when (out-this-line >= max-line-length)
        out-this-line := 0;
        for (i from 0 below $base64-line-break.size)
          result[out] := $base64-line-break[i];
          inc!(out);
        end;
      end;
      result[out] := byte;
      inc!(out);
    end,
    method outpad ()
      putbyte(fill-char);
    end,
    method out6 (six-bits :: <integer>)
      putbyte(encoding-vector[six-bits]);
    end,
    method out12 (twelve-bits :: <integer>)
      out6(ash(twelve-bits, -6));                    // shift right 6 bits
      out6(logand(twelve-bits, #b111111));
    end;
  block (continue)
    while (#t)
      let bytes-remaining :: <integer> = _end - in;
      select (bytes-remaining)
        0 => continue();
        1 => begin
               out12(first12(getbyte(), 0));
               outpad();
               outpad();
             end;
        2 => begin
               let byte1 :: <integer> = getbyte();
               let byte2 :: <integer> = getbyte();
               out12(first12(byte1, byte2));
               // is this right?  spec says right is padded with zeros.
               out6(ash(logand(byte2, #x0F), 2));
               outpad();
             end;
        otherwise => begin
                       let byte1 :: <integer> = getbyte();
                       let byte2 :: <integer> = getbyte();
                       let byte3 :: <integer> = getbyte();
                       out12(first12(byte1, byte2));
                       out12(second12(byte2, byte3));
                     end;
      end select;
    end while;
  end block;
  result
end base64-encode-string;

define function base64-decode-string
    (string, #key buffer, error?, standard-encoding?)
  let start :: <integer> = 0;
  let _end :: <integer> = string.size;
  let encoding-vector = iff(standard-encoding?,
                            $standard-encoding-vector
                            $http-encoding-vector);
  let fill-char = encoding-vector[64];
  let data-end = iterate pos (i :: <integer> = _end - 1)
                   iff (i > 0,
                        iff (string[i] = fill-char, pos(i - 1), i),
                        _end)
                 end;
  let length :: <integer> = ceiling/(3 * (data-end - start), 4);
  let result :: <byte-string> = buffer | make(<byte-string>, size: length);
  let out :: <integer> = 0;
  let in  :: <integer> = start;
  assert(~buffer | (buffer.size >= length),
         "Buffer not big enough to hold decoded base64 result.");
  local
    method getbyte ()
      unless (in >= _end)
        let byte = string[in];
        inc!(in);
        byte
      end
    end,
    // 11111122 22223333 33444444
    // 11111111 22222222 33333333
    method first8 (b1, b2)
      // All 6 bits from b1 in bits 7-2 of result.  bits 5-4 from b2 in 1-0.
      putbyte(logior(ash(b1, 2), ash(b2, -4)))
    end,
    method second8 (b2, b3)
      // Low 4 bits of b2 in bits 7-4 of result.  bits 5-2 from b3 in 3-0.
      putbyte(logior(ash(logand(b2, #b1111), 4), ash(b3, -2)))
    end,
    method third8 (b3, b4)
      // Low 2 buts of b3 in bits 7-6 of result.  All 6 bits from b4 in 5-0.
      putbyte(logior(ash(logand(b3, #b11), 6), b4))
    end,
    method putbyte (byte)
      when (out < length)
        result[out] := as(<byte-character>, byte);
        inc!(out);
      end;
    end,
    method get6 ()
      let val = 
      iterate loop (c = getbyte())
        let v = #f;
        case
          ~c
            => #f;
          c = fill-char
            => #"pad";
          v := position(encoding-vector, c)
            => v;
          as(<integer>, c) <= 32                // ignore control chars
            => loop(getbyte());
          otherwise => #f;
        end;
      end;
      val
    end;
  block (continue)
    while (#t)
      let b1 = get6() | continue();
      b1 = #"pad" & error("Pad seen in first byte of base64 encoding");
      let b2 = get6() | error("Odd base64 size.");
      iff(b2 = #"pad",
          begin
            first8(b1, 0);
            continue();
          end,
          first8(b1, b2));
      let b3 = get6() | error("Odd base64 size.");
      iff(b3 = #"pad",
          begin
            second8(b2, 0);
            continue();
          end,
          second8(b2, b3));
      let b4 = get6() | error("Odd base64 size.");
      iff(b4 = #"pad",
          begin
            third8(b3, 0);
            continue();
          end,
          third8(b3, b4));
    end while;
  end block;
  result
end base64-decode-string;
*/

