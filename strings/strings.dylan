Module:    strings
Synopsis:  String manipulation functions
Author:    Carl Gay
Copyright: This code is in the public domain.


/* TODO
  Conversion:
    as(<integer>, <string>)
    as(<string>, <integer>)
    as(<float>, <string>)
    as(<string>, <float>)
    as(<double-float>, <string>)
    as(<string>, <double-float>)
*/

//--------------------------------------------------------------------
// API

define open generic a-or-an (string :: <string>) => (article :: <string>);

define open generic pluralize (string :: <string>, #key) => (new-string :: <string>);

define open generic capitalize  (string :: <string>, #key) => (new-string :: <string>);
define open generic capitalize! (string :: <string>, #key) => (string :: <string>);

define open generic downcase  (string :: <string>, #key) => (new-string :: <string>);
define open generic downcase! (string :: <string>, #key) => (string :: <string>);

define open generic upcase  (string :: <string>, #key) => (new-string :: <string>);
define open generic upcase! (string :: <string>, #key) => (string :: <string>);

define open generic integer-to-digit (i :: <integer>, #key) => (c :: <character>);
define open generic digit-to-integer (c :: <character>, #key) => (i :: <integer>);

define open generic trim (string :: <string>, #key) => (new-string :: <string>);

define open generic join
    (items :: <sequence>, separator :: <string>, #key) => (new-string :: <string>);

define open generic split (string :: <string>, #key) => (words :: <sequence>);

define open generic replace
    (source :: <string>, old :: <string>, new :: <string>, #key)
 => (new-string :: <string>, num-replacements :: <integer>);

// These three are much like their counterparts (=, >, <) but they provide
// for keyword args.

define open generic equal?
    (s1 :: <object>, s2 :: <object>, #key, #all-keys) => (equal? :: <boolean>);
define open generic greater?
    (s1 :: <object>, s2 :: <object>, #key, #all-keys) => (greater? :: <boolean>);
define open generic less?
    (s1 :: <object>, s2 :: <object>, #key, #all-keys) => (less? :: <boolean>);

// These three aren't strictly necessary, given the test: keyword argument to
// the above three methods, but it seems common enough to want to pass these
// to other functions (e.g., sort) that it's worth the convenience.

// Is there a better name for these?

define open generic case-insensitive-equal?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (equal? :: <boolean>);
define open generic case-insensitive-greater?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (greater? :: <boolean>);
define open generic case-insensitive-less?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (less? :: <boolean>);


define open generic control?    (c :: <character>) => (well? :: <boolean>);
define open generic printable?  (c :: <character>) => (well? :: <boolean>);
define open generic graphic?    (c :: <character>) => (well? :: <boolean>);
define open generic lowercase?  (c :: <character>) => (well? :: <boolean>);
define open generic uppercase?  (c :: <character>) => (well? :: <boolean>);
define open generic whitespace? (c :: <character>) => (well? :: <boolean>);
define open generic alphanumeric? (c :: <character>) => (well? :: <boolean>);
define open generic digit?      (c :: <character>, #key) => (well? :: <boolean>);
define open generic alphabetic? (c :: <character>) => (well? :: <boolean>);

define open generic count-occurrances
    (source :: <string>, pattern :: <string>, #key) => (n :: <integer>);

define open generic index-of
    (source :: <string>, pattern :: <string>, #key)
 => (index :: false-or(<integer>));

//--------------------------------------------------------------------


// Temporary
define constant <byte-character> :: <class> = <character>;

// Temporary
define macro without-bounds-checks
    { without-bounds-checks ?:body end }
 => { begin ?body end }
end;

// Temporary
define class <invalid-index-error> (<simple-error>) end;

// Temporary
define function element-range-error
    (collection :: <collection>, key)
 => (will-never-return :: <bottom>)
  // We don't embed the collection in the condition as it will prevent the
  // collection having dynamic extent.  A debugger should be able to display
  // the collection.
  error(make(<invalid-index-error>,
             format-string: "ELEMENT outside of range: %=",
             format-arguments: list(key)))
end;


// Temporary
define inline function range-check
    (sequence :: <sequence>, _size :: <integer>, _start :: <integer>, _end :: <integer>) => ()
  when (_start < 0 | _start > _size)
    element-range-error(sequence, _start)
  end;
  when (_end < 0 | _end > _size)
    element-range-error(sequence, _end)
  end
end;




/// Utilities

define inline function uppercase-code?
    (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'A') <= code & code <= as(<integer>, 'Z')
end;

define inline function lowercase-code?
    (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'a') <= code & code <= as(<integer>, 'z')
end;

// default method
define method case-insensitive-less?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (less? :: <boolean>)
  o1 < o2
end;

define sealed method case-insensitive-less?
    (char1 :: <byte-character>, char2 :: <byte-character>, #key)
 => (less? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  when (lowercase-code?(code1))
    code1 := logxor(code1, #o40)
  end;
  when (lowercase-code?(code2))
    code2 := logxor(code2, #o40)
  end;
  code1 < code2
end method case-insensitive-less?;

define sealed method case-insensitive-less?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
	  start2 :: <integer> = 0, end2 :: <integer> = string2.size)
 => (less? :: <boolean>)
  less?(string1, string2, start1: start1, start2: start2, end1: end1, end2: end2,
        test: case-insensitive-less?)
end method case-insensitive-less?;


// default method
define method case-insensitive-greater?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (greater? :: <boolean>)
  o1 > o2
end;

define sealed method case-insensitive-greater? 
    (char1 :: <byte-character>, char2 :: <byte-character>, #key)
 => (greater? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  when (lowercase-code?(code1))
    code1 := logxor(code1, #o40)
  end;
  when (lowercase-code?(code2))
    code2 := logxor(code2, #o40)
  end;
  code1 > code2
end method case-insensitive-greater?;

define sealed method case-insensitive-greater?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
	  start2 :: <integer> = 0, end2 :: <integer> = string2.size)
 => (greater? :: <boolean>)
  greater?(string1, string2, start1: start1, start2: start2, end1: end1, end2: end2,
           test: case-insensitive-greater?)
end method case-insensitive-greater?;


// default method
define method case-insensitive-equal?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (equal? :: <boolean>)
  o1 = o2
end;

define sealed method case-insensitive-equal?
    (char1 :: <byte-character>, char2 :: <byte-character>, #key)
 => (equal? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  code1 == code2
  | (zero?(logand(#o337, logxor(code1, code2)))
     & (uppercase-code?(code1) | lowercase-code?(code1))
     & (uppercase-code?(code2) | lowercase-code?(code2)))
end;

define sealed method case-insensitive-equal?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
	  start2 :: <integer> = 0, end2 :: <integer> = string2.size)
 => (equal? :: <boolean>)
  equal?(string1, string2, start1: start1, start2: start2, end1: end1, end2: end2,
         test: case-insensitive-equal?)
end method case-insensitive-equal?;
  

// default method
define method equal?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (equal? :: <boolean>)
  o1 = o2
end;

define sealed method equal?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
	  start2 :: <integer> = 0, end2 :: <integer> = string2.size,
          test :: <function> = \==)
 => (equal? :: <boolean>)
  range-check(string1, size(string1), start1, end1);
  range-check(string2, size(string2), start2, end2);
  block (return)
    end1 - start1 = end2 - start2
    & without-bounds-checks
	for (i :: <integer> from start1 below end1,
	     j :: <integer> from start2 below end2)
	  let char1 :: <byte-character> = string1[i];
	  let char2 :: <byte-character> = string2[j];
	  unless (test(char1, char2))
	    return(#f)
	  end;
	finally
	  return(#t);
        end;
      end;
  end block;
end method equal?;

// default method
define method less?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (less? :: <boolean>)
  o1 < o2
end;

define sealed method less?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = size(string1),
	  start2 :: <integer> = 0, end2 :: <integer> = size(string2),
          test :: <function> = \==)
 => (less? :: <boolean>)
  range-check(string1, size(string1), start1, end1);
  range-check(string2, size(string2), start2, end2);
  let length1 = end1 - start1;
  let length2 = end2 - start2;
  let result = compare(string1, start1, string2, start2, min(length1, length2), test);
  if (result = 0)
    length1 < length2
  else
    result < 0
  end
end method less?;


// default method
define method greater?
    (o1 :: <object>, o2 :: <object>, #key, #all-keys) => (greater? :: <boolean>)
  o1 > o2
end;

define sealed method greater?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = size(string1),
	  start2 :: <integer> = 0, end2 :: <integer> = size(string2),
          test :: <function> = \==)
 => (greater? :: <boolean>)
  range-check(string1, size(string1), start1, end1);
  range-check(string2, size(string2), start2, end2);
  let length1 = end1 - start1;
  let length2 = end2 - start2;
  let result = compare(string1, start1, string2, start2, min(length1, length2), test);
  if (result = 0)
    length1 > length2
  else
    result > 0
  end
end method greater?;

define sealed method compare
    (string1 :: <byte-string>, start1 :: <integer>, 
     string2 :: <byte-string>, start2 :: <integer>,
     count :: <integer>, test :: <function>)
 => (result :: <integer>)
  let subrange1 = size(string1) - start1;
  let subrange2 = size(string2) - start2;
  let state = 0;
  case
    count > subrange1 =>
      case
	count > subrange2 =>
	  count := min(subrange1, subrange2);
	  state := 1;
	otherwise =>
	  count := subrange1;
	  state := 2
      end;
    count > subrange2 =>
      count := subrange2;
      state := 3
  end case;
  block (return)
    without-bounds-checks
      for (i1 :: <integer> = start1 then i1 + 1,
	   i2 :: <integer> = start2 then i2 + 1,
	   until: count = 0)
	let char1 :: <byte-character> = string1[i1];
	let char2 :: <byte-character> = string2[i2];
	unless (test(char1, char2))
	  return(if (case-insensitive-less?(char1, char2))
		   (start1 - i1) - 1
		 else
		   (i1 + 1) - start1
		 end)
	end;
	count := count - 1;
      finally
	select (state)
	  0 => 0;
	  1 => case
		 subrange1 = subrange2 => 0;
		 subrange1 < subrange2 => -1 - i1;
		 otherwise => i1 + 1
	       end;
	  2 => (start1 - i1) - 1;
	  otherwise => (i1 - start1) + 1
	end
      end for
    end without-bounds-checks
  end block
end method compare;

/// Predicates

define sealed method alphabetic?
    (char :: <byte-character>) => (alpha? :: <boolean>)
  let code = as(<integer>, char);
  uppercase-code?(code) | lowercase-code?(code)
end;

define sealed method digit?
    (char :: <byte-character>, #key base :: <integer> = 10)
 => (digit? :: <boolean>)
  check-base(base);
  let code = as(<integer>, char);
  let zero = as(<integer>, '0');
  if (base <= 10)
    zero <= code & code < zero + base
  else
    let big-a :: <integer> = as(<integer>, 'A');
    let small-a :: <integer> = as(<integer>, 'a');
    let radix :: <integer> = base - 10;
    (zero <= code & code <= as(<integer>, '9'))
      | (big-a <= code & code < big-a + radix)
      | (small-a <= code & code < small-a + radix)
  end if;
end method digit?;

/*
for (base in #(2,8,10,16,36))
  let p = "/0123456789abcdefghijklmnopqrstuvwxyz.";
  for (digit in list(p[0], p[1], p[base], p[base + 1]))
    add!(r, pair(digit, digit?(digit, base: base)));
  end;
end;
*/

define sealed method alphanumeric?
    (char :: <byte-character>) => (alphanum? :: <boolean>)
  alphabetic?(char) | digit?(char)
end;


define sealed method uppercase?
    (char :: <byte-character>) => (upper? :: <boolean>)
  let code = as(<integer>, char);
  uppercase-code?(code)
end;

define sealed method lowercase?
    (char :: <byte-character>) => (lower? :: <boolean>)
  let code = as(<integer>, char);
  lowercase-code?(code)
end;


// Returns #t iff the character is a "graphic" (printing) character
define sealed method graphic?
    (char :: <byte-character>) => (graphic? :: <boolean>)
  let code = as(<integer>, char);
  as(<integer>, ' ') <= code & code <= as(<integer>, '~')
end;

// isprint
//
define sealed method printable?
    (c :: <character>) => (printable? :: <boolean>)
  graphic?(c) | whitespace?(c);
end;

// ispunct
//
define sealed method punctuation?
    (c :: <byte-character>) => (punct? :: <boolean>)
  select (c)
    '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*',
    '+', ',', '-', '.', '/', ':', ';', '<', '=', '>',
    '?', '@', '[', '\\', ']', '^', '_', '`', '{', '|', '}', '~'
      => #t;
    otherwise => #f;
  end select;
end method punctuation?;

define sealed method control?
    (c :: <byte-character>) => (ctrl? :: <boolean>)
  ~ printable?(c);
end;

define sealed method whitespace?
    (c :: <byte-character>) => (white? :: <boolean>)
  select (c)
    // Space, tab, newline, formfeed, carriage return
    ' ', '\t', '\n', '\f', '\r' => #t;
    otherwise => #f;
  end select;
end method whitespace?;

/// Other string utilities

define sealed method capitalize
    (string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (string :: <byte-string>)
  capitalize!(copy-sequence(string, start: _start, end: _end))
end;

define sealed method capitalize!
    (string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = string.size)
 => (string :: <byte-string>)
  range-check(string, size(string), _start, _end);
  let state = #f;
  without-bounds-checks
    for (i :: <integer> from _start below _end)
      let char :: <byte-character> = string[i];
      case
	~state =>		// between words
	  case
	    alphabetic?(char) =>
	      string[i] := as-uppercase(char);
	      state := #t;
	    digit?(char) =>
	      state := #t;
	  end;
	otherwise =>
	  case
	    alphabetic?(char) =>
	      string[i] := as-lowercase(char);
	    ~digit?(char) =>
	      state := #f;
	  end;
      end
    end
  end without-bounds-checks;
  string
end method capitalize!;

define sealed method upcase
    (string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (string :: <byte-string>)
  do-string!(copy-sequence(string, start: _start, end: _end),
             as-uppercase, _start, _end)
end method upcase;

define sealed method upcase!
    (string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = string.size)
 => (string :: <byte-string>)
  do-string!(string, as-uppercase, _start, _end)
end method upcase!;

define sealed method downcase
    (string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = string.size)
 => (string :: <byte-string>)
  do-string!(copy-sequence(string, start: _start, end: _end),
             as-lowercase, _start, _end)
end method downcase;

define sealed method downcase!
    (string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (string :: <byte-string>)
  do-string!(string, as-lowercase, _start, _end)
end method downcase!;

define sealed method do-string!
    (string :: <byte-string>, fun :: <function>, _start :: <integer>, _end :: <integer>)
 => (string :: <byte-string>)
  range-check(string, string.size, _start, _end);
  without-bounds-checks
    for (i :: <integer> from _start below _end)
      string[i] := fun(string[i]);
    end
  end;
  string
end method do-string!;

/// Cool string hacks

// Pluralize the given string
define method pluralize
    (string :: <byte-string>, #key count) => (plural :: <byte-string>)
  let length :: <integer> = size(string);
  let pos    :: <integer> = (string-search-set(string, #[' ', '\t'], from-end?: #t) | -1) + 1;
  if (length = 0 | (count & count = 1))
    string
  else
    let flush  = #f;
    let suffix = #f;
    let last-char   :: <byte-character> = string[length - 1];
    let penult-char :: <byte-character>
      = if (length > 1) string[length - 2] else '*' end;
    local method find-char (chars :: <byte-string>, char :: <byte-character>)
	    member?(char, chars, test: case-insensitive-equal?)
	  end method;
    case
      case-insensitive-equal?(last-char, 'y')
      & ~find-char("aeiou", penult-char) =>
	flush  := 1;
	suffix := "ies";
      equal?(string, "ox", start1: pos)
      | equal?(string, "vax", start1: pos) =>
	suffix := "en";
      length >= 5
      & equal?(string, "index", start1: length - 5) =>
	flush  := 2;
	suffix := "ices";
      (case-insensitive-equal?(last-char, 'h') & find-char("cs", penult-char))
      | find-char("szx", last-char) =>
	suffix := "es";
      length >= 3
      & equal?(string, "man", start1: length - 3)
      & ~equal?(string, "human", start1: pos) =>
	flush  := 2;
	suffix := "en";
      length >= 3
      & equal?(string, "ife", start1: length - 3) =>
	flush  := 2;
	suffix := "ves";
      length >= 5
      & equal?(string, "child", start1: length - 5) =>
	suffix := "ren";
      otherwise =>
	suffix := "s";
    end case;
    concatenate-as(<byte-string>,
		   if (flush) copy-sequence(string, start: 0, end: length - flush)
		   else string end,
		   suffix)
  end if
end method pluralize;

// Returns an article to be used with the specified string
// We admittedly heuristicate...
define method a-or-an
    (string :: <byte-string>) => (article :: <byte-string>)
  let length :: <integer> = size(string);
  if (length = 0)
    ""
  else
    let char :: <byte-character> = string[0];
    local method find-char (chars :: <byte-string>, char :: <byte-character>)
            member?(char, chars, test: case-insensitive-equal?)
          end method;
    case
      equal?(string, "one")
      | (length >= 4 & equal?(string, "one ", end1: 4)) =>
        "a ";
      length = 1
      // "an x", but "a xylophone"
      // "an fff", but "a frog"
      | ~string-search-set(string, "aeiou")
      // "an xl400", but "a xylophone"
      | string-search-set(string, "0123456789") =>
        if (find-char("aefhilmnorsx", char)) "an " else "a " end;
      otherwise =>
        if (find-char("aio", char)
            // "an egg", but "a eunich"
            | (case-insensitive-equal?(char, 'e')
               & ~equal?(string, "eu", end1: 2))
            // "an umbrella", but "a unicorn"
            // "a uniform", but "an uninformed ..."
            // And of course, "a unix"
            | (case-insensitive-equal?(char, 'u')
               & ~(equal?(string, "uni", end1: 3)
                   & (length < 5
                      // Treat "y" as a vowel here, e.g., "unicycle"
                      | ~find-char("bcdfghjklmnpqrstvwxz", string[4])))))
          "an "
        else
          "a "
        end if
    end case
  end if
end method a-or-an;

// Find any of the given characters within a string
define method string-search-set
    (string :: <byte-string>, char-set :: <sequence>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = string.size,
	  from-end? :: <boolean> = #f, test = case-insensitive-equal?)
 => (index :: false-or(<integer>))
  range-check(string, size(string), _start, _end);
  block (return)
    let set-length :: <integer> = size(char-set);
    without-bounds-checks
      if (from-end?)
	for (i :: <integer> = _end - 1 then i - 1,
	     until: i < _start)
	  let char :: <byte-character> = string[i];
	  for (j :: <integer> = 0 then j + 1,
	       until: j >= set-length)
	    when (test(char, char-set[j]))
	      return(i)
	    end;
	  finally #f;
	  end;
	finally #f;
	end
      else
	for (i :: <integer> = _start then i + 1,
	     until: i >= _end)
	  let char :: <byte-character> = string[i];
	  for (j :: <integer> = 0 then j + 1,
	       until: j >= set-length)
	    when (test(char, char-set[j]))
	      return(i)
	    end;
	  finally #f;
	  end;
	finally #f;
	end for
      end if
    end without-bounds-checks
  end block
end method string-search-set;


define sideways method as
    (the-class == <string>, c :: <character>) => (c :: <string>)
  make(<string>, size: 1, fill: c)
end;


define function check-base
    (base :: <integer>) => ();
  if (base < 2 | base > 36)
    error("%d is not a legal base.", base); // TODO: <invalid-argument-error>
  end if;
end;


// Converts an integer into a character.  Bases beyond 36 probably
// don't come out very well, however.
//
define method integer-to-digit
    (integer :: <integer>, #key base :: <integer> = 10, uppercase = #f)
 => (digit :: <character>)
  check-base(base);
  if (integer < 0   |   integer >= base)
    error("%d isn't a digit in base %d", integer, base); // TODO: <invalid-argument-error>
  end if;

  select (integer)
    0 => '0';
    1 => '1';
    2 => '2';
    3 => '3';
    4 => '4';
    5 => '5';
    6 => '6';
    7 => '7';
    8 => '8';
    9 => '9';
    otherwise =>
      as(<character>, 
	 integer - 10 + as(<integer>, if (uppercase) 'A' else 'a' end));
  end select;
end method integer-to-digit;

// Base is provided solely for error-checking the input, c.
define method digit-to-integer
    (c :: <character>, #key base :: <integer> = 10)
 => (int :: <integer>)
  check-base(base);
  let int = select (c)
              '0' => 0;
              '1' => 1;
              '2' => 2;
              '3' => 3;
              '4' => 4;
              '5' => 5;
              '6' => 6;
              '7' => 7;
              '8' => 8;
              '9' => 9;
              otherwise =>
                as(<integer>, as-lowercase(c)) - as(<integer>, 'a') + 10;
            end;
  if (int < 0 | int >= base)
    error("%d isn't a digit in base %d", int, base); // TODO: <invalid-argument-error>
  end;
  int
end method digit-to-integer;

// ----------------------------------------------------------------------
// join(range(from: 1, to: 3), ", ",
//      key: integer-to-string,
//      conjunction: " and ");
// => "1, 2 and 3"

define method join
    (seq :: <sequence>, separator :: <string>,
     #key key :: <function> = identity,
         conjunction :: false-or(<string>))
 => (result :: <string>)
  with-output-to-string (out)
    let len-1 :: <integer> = seq.size - 1;
    for (i :: <integer> from 1,
         item in seq)
      let v = key(item);
      select (v by instance?)
        <string> => write(out, v);
        <character> => write-element(out, v);
        otherwise => write(out, as(<string>, v));
      end;
      //write(out, as(<string>, key(item)));
      if (i < len-1)
        write(out, separator);
      elseif (i == len-1)
        write(out, conjunction | separator);
      end;
    end;
  end
end method join;



// Hopefully this can replace 'split' in common-extensions.

define method split
    (string :: <byte-string>,
     #key separator :: false-or(<byte-string>),
          start :: <integer> = 0,
          end: _end :: <integer> = string.size,
          trim? :: <boolean> = #t,
          max: max-splits :: false-or(<integer>),
          allow-empty-strings? :: <boolean>)
 => (strings :: <stretchy-object-vector>)
  local method separator? (pos :: <integer>)
          block (return)
            for (i :: <integer> from pos, c in separator)
              if (i >= _end | string[i] ~== c)
                return(#f);
              end;
            end;
            #t
          end
        end,
        method is-white? (pos :: <integer>)
          whitespace?(string[pos])
        end;
  splitf(string,
         if (separator) separator? else is-white? end,
         if (separator) size(separator) else 1 end,
         start: start,
         end: _end,
         trim?: trim?,
         max: max-splits,
         allow-empty-strings?: allow-empty-strings?)
end method split;
           
define method splitf
    (string :: <byte-string>, separator? :: <function>, separator-size :: <integer>,
     #key start :: <integer> = 0, 
          end: epos :: <integer> = size(string),
          trim? :: <boolean> = #t,
          max: max-splits :: false-or(<integer>),
          allow-empty-strings? :: <boolean>)
  let bpos :: <integer> = start;
  let new-pos :: <integer> = bpos;
  let parts :: <stretchy-vector> = make(<stretchy-vector>);
  local method add-substring
                   (start :: <integer>, _end :: <integer>)
	  if (trim?)
	    while (start < _end & whitespace?(string[start]))
	      start := start + 1
	    end;
	    while (start < _end & whitespace?(string[_end - 1]))
	      _end := _end - 1
	    end
	  end;
	  if (allow-empty-strings? | start ~== _end)
	    add!(parts, copy-sequence(string, start: start, end: _end))
	  end
	end method add-substring;
  let splits :: <integer> = 0;
  while (new-pos < epos & (~max-splits | splits < max-splits))
    if (separator?(new-pos))
      add-substring(bpos, new-pos);
      if (allow-empty-strings?)
        new-pos := new-pos + separator-size;
      else
        // skip consecutive separators
        while (new-pos < epos & separator?(new-pos))
          new-pos := new-pos + separator-size;
        end;
      end;
      bpos := new-pos;
      splits := splits + 1;
    else
      new-pos := new-pos + 1;
    end if;
  end while;
  add-substring(bpos, epos);
  parts
end method splitf;

//split("1,2,,4", separator: ",", allow-empty-strings?: #t);


/*
define sealed method looking-at?
    (pat :: <byte-string>, buf :: <byte-string>, bpos :: <integer>, epos :: <integer>)
  let pend = bpos + pat.size;
  pend <= epos & string-match(pat, buf, bpos, pend)
end looking-at?;
*/

define sealed method count-occurrances
    (source :: <byte-string>, pattern :: <byte-string>,
     #key test :: <function> = \==,
          start :: <integer> = 0,
          end: _end :: <integer> = source.size)
 => (occurrances :: <integer>)
  let psize :: <integer> = pattern.size;
  let i :: <integer> = start;
  let occurrances :: <integer> = 0;
  while (i + psize <= _end)
    if (equal?(source, pattern, start1: i, end1: i + psize, test: test))
      occurrances := occurrances + 1;
      i := i + psize;  // overlapping occurrances aren't counted.
    else
      i := i + 1;
    end;
  end;
  occurrances
end method count-occurrances;

define sealed method trim
    (string :: <byte-string>,
     #key test :: <function> = whitespace?,
          side :: one-of(#"left", #"right", #"both") = #"both",
          start :: <integer> = 0,
          end: _end :: <integer> = string.size)
 => (trimmed-string :: <byte-string>)
  let bpos :: <integer> = start;
  let epos :: <integer> = _end;
  if (side == #"both" | side == #"left")
    while (bpos < epos & test(string[bpos]))
      bpos := bpos + 1;
    end;
  end;
  if (side == #"both" | side == #"right")
    while (bpos < (epos - 1) & test(string[epos - 1]))
      epos := epos - 1;
    end;
  end;
  // If strings were immutable, could optimize out the copy sometimes.
  copy-sequence(string, start: bpos, end: epos)
end method trim;

define sealed method index-of
    (source :: <byte-string>, pattern :: <byte-string>,
     #key test :: <function> = \==,
          from-end? :: <boolean> = #f,
          start :: <integer> = 0,
          end: _end :: <integer> = source.size)
 => (index :: false-or(<integer>))
  let psize :: <integer> = pattern.size;
  let step :: <integer> = if (from-end?) -1 else 1 end;
  let index :: <integer> = if (from-end?) _end - psize else start end;
  block (return)
    while (index >= start & index + psize <= _end)
      if (equal?(source, pattern, start1: index, end1: index + psize, test: test))
        return(index)
      end;
      index := index + step;
    end;
    #f
  end block
end method index-of;

define sealed method replace
    (source :: <byte-string>, old :: <byte-string>, new :: <byte-string>,
     #key start :: <integer> = 0,
          end: _end :: <integer> = source.size,
          test :: <function> = \==,
          max: max-replacements :: false-or(<integer>) = #f)
 => (string :: <byte-string>, num-replacements :: <integer>)
  let osize :: <integer> = old.size;
  let nsize :: <integer> = new.size;
  let bpos :: <integer> = start;
  let saved-pos :: <integer> = bpos;
  let num-replacements :: <integer> = 0;
  values(with-output-to-string (stream)
           while (bpos + osize <= _end & num-replacements < max-replacements)
             if (equal?(source, old, start1: bpos, end1: bpos + osize, test: test))
               if (saved-pos < bpos)
                 // TODO: get rid of call to copy-sequence.  Might benchmark using count-occurrances,
                 //       then pre-allocating a string of the correct size.
                 write(stream, copy-sequence(source, start: saved-pos, end: bpos));
               end;
               write(stream, new);
               bpos := bpos + osize;
               saved-pos := bpos;
             else
               bpos := bpos + 1;
             end if;
           end while;
           if (saved-pos < _end)
             write(stream, copy-sequence(source, start: saved-pos, end: _end))
           end;
         end with-output-to-string,
         num-replacements)
end method replace;

