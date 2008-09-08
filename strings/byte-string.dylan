Module:    strings-implementation
Author:    Gail Zacharias, Carl Gay
Synopsis:  Low-level string utilities designed to be as fast as possible.
           This code assumes <byte-string>s only.  It was originally written
           for use in the HTTP server.  (Note that a different definition of
           whitespace is used in this file.)


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
define inline function %whitespace? (ch :: <byte-character>)
  ch == '\t' | ch == ' '
end;

define function whitespace-position (buf :: <byte-string>,
                                     bpos :: <integer>,
                                     epos :: <integer>)
  => (pos :: false-or(<integer>))
  char-position-if(%whitespace?, buf, bpos, epos);
end whitespace-position;

define function skip-whitespace (buffer :: <byte-string>,
                                 bpos :: <integer>,
                                 epos :: <integer>)
  => (pos :: <integer>)
  iterate fwd (pos :: <integer> = bpos)
    if (pos >= epos | ~%whitespace?(buffer[pos]))
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
             if (last >= start & %whitespace?(buffer[last]))
               bwd(last)
             else
               epos
             end;
           end;
         end)
end trim-whitespace;

/*
define function %trim
    (string :: <byte-string>) => (trimmed-string :: <byte-string>)
  let len :: <integer> = size(string);
  let (bpos, epos) = trim-whitespace(string, 0, len);
  if (bpos == 0 & epos == len)
    string
  else
    copy-sequence(string, start: bpos, end: epos)
  end;
end;
*/

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
        return(i);
      end;
    end;
  end
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
    if (n <= 9)
      n
    else
      let n = n - (as(<integer>, 'a') - as(<integer>, '0') - 10);
      10 <= n & n <= 15 & n
    end;
  end;
end digit-weight;


