Module:    utilities
Synopsis:  String utilities
Author:    Gail Zacharias, Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define constant $cr = as(<character>, 13);  // \r
define constant $lf = as(<character>, 10);  // \n

define inline function char-position-if (test? :: <function>,
                                         buf :: <byte-string>,
                                         bpos :: <integer>,
                                         epos :: <integer>)
  => (pos :: false-or(<integer>))
  iterate loop (pos :: <integer> = bpos)
    unless (pos == epos)
      if (test?(buf[pos])) pos else loop(pos + 1) end;
    end;
  end;
end;

define function char-position (ch :: <byte-character>,
                               buf :: <byte-string>,
                               bpos :: <integer>,
                               epos :: <integer>)
  => (pos :: false-or(<integer>))
  char-position-if(method(c) c == ch end, buf, bpos, epos);
end char-position;

define function char-position-from-end (ch :: <byte-character>,
                                        buf :: <byte-string>,
                                        bpos :: <integer>,
                                        epos :: <integer>)
  => (pos :: false-or(<integer>))
  iterate loop (pos :: <integer> = epos)
    unless (pos == bpos)
      let npos = pos - 1;
      if (ch == buf[npos]) npos else loop(npos) end;
    end;
  end;
end char-position-from-end;

// Note that this doesn't check for stray cr's or lf's, because
// those are just random control chars, proper crlf's got
// eliminated during header reading.
define inline function whitespace? (ch :: <byte-character>)
  ch == '\t' | ch == ' '
end;

define function whitespace-position (buf :: <byte-string>,
                                     bpos :: <integer>,
                                     epos :: <integer>)
  => (pos :: false-or(<integer>))
  char-position-if(whitespace?, buf, bpos, epos);
end whitespace-position;

define function skip-whitespace (buffer :: <byte-string>,
                                 bpos :: <integer>,
                                 epos :: <integer>)
  => (pos :: <integer>)
  iterate fwd (pos :: <integer> = bpos)
    if (pos >= epos | ~whitespace?(buffer[pos]))
      pos
    else
      fwd(pos + 1)
    end;
  end;
end skip-whitespace;

define function trim-whitespace (buffer :: <byte-string>,
                                 start :: <integer>,
                                 endp :: <integer>)
  => (start :: <integer>, endp :: <integer>)
  let pos = skip-whitespace(buffer, start, endp);
  values(pos,
         if (pos == endp)
           endp
         else
           iterate bwd (epos :: <integer> = endp)
             let last = epos - 1;
             if (last >= start & whitespace?(buffer[last]))
               bwd(last)
             else
               epos
             end;
           end;
         end)
end trim-whitespace;

// Ugh. should look up in a table...
define inline function non-token-char? (ch :: <byte-character>)
  let c = as(<integer>, ch);
  c <= 32 | c >= 127 |
  c == as(<integer>, '"') |
  c == as(<integer>, '(') |
  c == as(<integer>, ')') |
  c == as(<integer>, ',') |
  c == as(<integer>, '/') |
  c == as(<integer>, '/') |
  c == as(<integer>, '{') |
  c == as(<integer>, '}') |
  (as(<integer>, ':') <= c & c <= as(<integer>, '@')) |  // ":;<=>?@"
  (as(<integer>, '[') <= c & c <= as(<integer>, ']'))   // "[\]"
end;

define function token-end-position (buf :: <byte-string>,
                                    bpos :: <integer>,
                                    epos :: <integer>)
  char-position-if(non-token-char?, buf, bpos, epos)
end;

define inline function looking-at? 
    (pat :: <byte-string>, buf :: <byte-string>, bpos :: <integer>, epos :: <integer>)
  let pend = bpos + pat.size;
  pend <= epos & string-match(pat, buf, bpos, pend)
end looking-at?;


define inline function key-match (key :: <symbol>,
                                  buf :: <byte-string>,
                                  bpos :: <integer>,
                                  epos :: <integer>)
  string-match(as(<string>, key), buf, bpos, epos)
end key-match;

define function string-match (str :: <byte-string>,
                              buf :: <byte-string>,
                              bpos :: <integer>,
                              epos :: <integer>)
  string-equal-2(str, 0, str.size, buf, bpos, epos - bpos);
end string-match;

// Find the small string in the big string, starting at bpos in big and ending at epos in big.
define function string-position 
    (big :: <byte-string>, small :: <byte-string>, bpos :: <integer>, epos :: <integer>)
 => (pos :: false-or(<integer>))
  block (return)
    let len = size(small);
    for (i from bpos to (epos - len))
      when (string-equal-2(big, i, len, small, 0, len))
        return(i); end; end; end;
end string-position;

define method string-equal? (s1 :: <substring>, s2 :: <substring>)
  string-equal-2(s1.substring-base, s1.substring-start, s1.size,
                 s2.substring-base, s2.substring-start, s2.size)
end;

define method string-equal? (s1 :: <substring>, s2 :: <byte-string>)
  string-equal-2(s1.substring-base, s1.substring-start, s1.size,
                 s2, 0, s2.size)
end;

define method string-equal? (s1 :: <byte-string>, s2 :: <substring>)
  string-equal-2(s1, 0, s1.size,
                 s2.substring-base, s2.substring-start, s2.size)
end;

define method string-equal? (s1 :: <byte-string>, s2 :: <byte-string>)
  string-equal-2(s1, 0, s1.size, s2, 0, s2.size)
end;

define inline function string-equal-2 (s1 :: <byte-string>,
                                       bpos1 :: <integer>,
                                       len1 :: <integer>,
                                       s2 :: <byte-string>,
                                       bpos2 :: <integer>,
                                       len2 :: <integer>)
  when (len1 == len2)
    let epos1 :: <integer> = bpos1 + len1;
    iterate loop(pos1 :: <integer> = bpos1, pos2 :: <integer> = bpos2)
      pos1 == epos1 |
        (as-lowercase(s1[pos1]) == as-lowercase(s2[pos2]) & loop(pos1 + 1, pos2 + 1))
    end;
  end;
end string-equal-2;

define function digit-weight (ch :: <byte-character>) => (n :: false-or(<integer>))
  when (ch >= '0')
    let n = logior(as(<integer>, ch), 32) - as(<integer>, '0');
    if (n <= 9) n
    else
      let n = n - (as(<integer>, 'a') - as(<integer>, '0') - 10);
      10 <= n & n <= 15 & n
    end;
  end;
end digit-weight;

//define constant ($maximum-integer-q, $maximum-integer-r) = floor/($maximum-integer, 10);

// Returns #f if the string is empty, or there are any non-digits in string, or
// if value would exceed an <integer>.  Maybe should return $maximum-integer
// in the latter case?
define function string->integer
    (buf :: <byte-string>, bpos :: <integer>, epos :: <integer>)
 => (value :: false-or(<integer>));
  /*
  iterate loop (pos :: <integer> = bpos, value :: <integer> = 0)
    if (pos == epos)
      bpos ~== epos & value
    else
      let n = as(<integer>, buf[pos]) - as(<integer>, '0');
      when (0 <= n & ((value < $maximum-integer-q & n <= 9) |
                        (value == $maximum-integer-q & n <= $maximum-integer-r)))
        loop(pos + 1, value * 10 + n);
      end;
    end;
  end iterate;
  */
  string-to-integer(buf, start: bpos, end: epos, default: #f)
end string->integer;


