Module:    utilities
Synopsis:  Non-copying substrings.
Author:    Gail Zacharias, Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define class <small-substring> (<string>, <sealed-constructor>)
  constant slot substring-base :: <byte-string>,
    required-init-keyword: base:;
  // Encodes start-pos in upper bits and length in lower bits.
  constant slot substring-range :: <integer>,
    required-init-keyword: range:;
end class;

// Some day, might support big substrings with non-integer range...
define constant <substring> = <small-substring>;

//define constant $substring-overhead :: <integer> = 8;
define constant $substring-bits :: <integer> = 14;
define constant $substring-end-mask :: <integer> = ash(1, $substring-bits) - 1;
define constant $substring-start-shift :: <integer> = (- $substring-bits);


define sealed inline method type-for-copy
    (str :: <substring>) => (type :: singleton(<byte-string>))
  <byte-string>
end;

define sealed inline method element-type
    (str :: <substring>) => (type :: singleton(<byte-character>))
  <byte-character>
end;

/*
define open method copy-bytes
    (src :: <substring>, src-start :: <integer>, dst :: <byte-string>,
     dst-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := src[src-start + i]
  end
end method;
*/

/*
define inline function make-substring-range
    (begp :: <integer>, endp :: <integer>) => (range :: false-or(<integer>))
  let len = endp - begp;
  when (len >= $substring-overhead
        & begp < ash(1, 28 - $substring-bits)  // 28 non-sign bits in FD?
        & len < ash(1, $substring-bits))
    ash(begp, $substring-bits) + endp
  end;
end;
*/

define inline function substring-start (ss :: <substring>) => (start :: <integer>)
  ash(ss.substring-range, $substring-start-shift)
end;

define inline function substring-end (ss :: <substring>) => (endp :: <integer>)
  logand(ss.substring-range, $substring-end-mask)
end;

define inline sealed method size (ss :: <substring>) => (sz :: <integer>)
  let range = ss.substring-range;
  logand(range, $substring-end-mask) - ash(range, $substring-start-shift)
end;

// Can't use <substring>s until the next release of FunDev is out (i.e., a post 2.0 SP1
// release) because of a bug in copy-bytes that is triggered when you try to use
// format(my-string-stream, "%s", my-substring)
define method substring
    (str :: <byte-string>, begp :: <integer>, endp :: <integer>)
  iff(begp == 0 & endp == str.size,
      str,
      copy-sequence(str, start: begp, end: endp))
end;

/* TODO: see comment above
define sealed method substring
    (str :: <byte-string>, begp :: <integer>, endp :: <integer>)
  if (begp == 0 & endp == str.size)
    str
  else
    let range = make-substring-range(begp, endp);
    if (range)
      make(<substring>, base: str, range: range)
    else
      copy-sequence(str, start: begp, end: endp)
    end;
  end;
end;

define sealed method substring
    (str :: <substring>, begp :: <integer>, endp :: <integer>)
  let offset = str.substring-start;
  // should this use >= ?  As it stands, a substring can be longer than its "parent".
  if (begp == 0 & endp == str.size)
    str
  else
    substring(str.substring-base, offset + begp, offset + endp)
  end;
end;
*/

define generic string-extent (str :: <string>)
   => (bstr :: <byte-string>, bpos :: <integer>, epos :: <integer>);

define method string-extent (str :: <byte-string>)
   => (bstr :: <byte-string>, bpos :: <integer>, epos :: <integer>);
  values(str, 0, size(str))
end;

define method string-extent (str :: <substring>)
   => (bstr :: <byte-string>, bpos :: <integer>, epos :: <integer>);
  values(str.substring-base, str.substring-start, str.substring-end)
end;

define inline sealed method forward-iteration-protocol
    (ss :: <substring>)
  => (initial-state :: <object>, limit :: <object>,
      next-state :: <function>,
      finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>,
      current-element-setter :: <function>,
      copy-state :: <function>);
  values(ss.substring-start,
         ss.substring-end,
         method (ss, i :: <integer>)
           i + 1
         end,
         method (ss, i :: <integer>, limit)
           i == limit
         end,
         method (ss :: <substring>, i :: <integer>)
           i - ss.substring-start
         end,
         method (ss :: <substring>, i :: <integer>)
           ss.substring-base[i]
         end,
         method (c :: <byte-character>, ss :: <substring>, i :: <integer>)
           ss.substring-base[i] := c
         end,
         method (ss, i) i end)
end forward-iteration-protocol;

define sealed method element (ss :: <substring>, index :: <integer>, #key default = unsupplied())
  => (ch-or-default)
  if (element-range-check(index, ss.size))
    element-no-bounds-check(ss, index)
  elseif (supplied?(default))
    default
  else
    element-range-error(ss, index);
  end
end;

define sealed inline method element-no-bounds-check
    (ss :: <substring>, index :: <integer>, #key default = unsupplied())
  => (ch-or-default)
  if (supplied?(default))
    element-no-bounds-check(ss.substring-base, ss.substring-start + index, default: default)
  else
    element-no-bounds-check(ss.substring-base, ss.substring-start + index)
  end
end;

define sealed method element-setter (c :: <byte-character>, ss :: <substring>, index :: <integer>)
  => (c :: <byte-character>)
  if (element-range-check(index, ss.size))
    element-no-bounds-check-setter(c, ss, index)
  else
    element-range-error(ss, index)
  end;
end;

define sealed inline method element-no-bounds-check-setter
    (c :: <byte-character>, ss :: <substring>, index :: <integer>) => (c :: <byte-character>)
   element-no-bounds-check-setter(c, ss.substring-base, ss.substring-start + index)
end;

/* for efficiency?
define method \=
    (s1 :: <substring>, s2 :: <substring>) => (equal? :: <boolean>)
  s1 == s2
  | begin
      let len = size(s1);
      len == size(s2) & ss=-internal(substring-base(s1), substring-start(s1),
                                     substring-base(s2), substring-start(s2), len)
    end
end;

define method \=
    (bs :: <byte-string>, ss :: <substring>) => (equal? :: <boolean>)
  let len = size(bs);
  len == size(ss) & ss=-internal(bs, 0, substring-base(ss), substring-start(ss), len)
end;

define method \=
    (ss :: <substring>, bs :: <byte-string>) => (equal? :: <boolean>)
  let len = size(bs);
  len == size(ss) & ss=-internal(substring-base(ss), substring-start(ss), bs, 0, len)
end;

define inline method ss=-internal
    (s1 :: <byte-string>, i1 :: <integer>, s2 :: <byte-string>, i2 :: <integer>, len :: <integer>)
 => (same? :: <boolean>)
  case
    len == 0         => #t;
    s1[i1] == s2[i2] => ss=-internal(i1 + 1, i2 + 1, len - 1);
    otherwise        => #f;
  end
end;
*/

define method as
    (class == <byte-string>, ss :: <substring>) => (bs :: <byte-string>)
  copy-sequence(substring-base(ss), start: substring-start(ss), end: substring-end(ss))
end;

