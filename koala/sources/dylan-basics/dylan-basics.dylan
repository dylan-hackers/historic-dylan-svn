Module:   dylan-basics
Synopsis: Basic dylan utilities
Author:   many authors


// This library is a collection of utilities that are generally useful
// for Dylan programming, but aren't big enough to have a library all
// their own.  These were written by various authors and shamelessly
// stolen (or written) by me (Carl Gay).


// ---TODO: Add an equal? method, that is like = but does case insensitive
//          string comparison.  Need string-equal? first.

// ----------------------------------------------------------------------
// bind introduces new bindings a la "let", but also introduces a new
// block to limit the variables' scope.
//
// bind (x = 1, y :: <string> = "y")
//   x + y
// end
//
define macro bind
    { bind (?bindings) ?:body end }
 => { begin
        ?bindings
        ;
        ?body
      end }
bindings:
    { } => { }
    { ?binding, ... } => { ?binding; ... }
binding:
    { ?var:variable = ?val:expression }
 => { let ?var = ?val }
end;

// ----------------------------------------------------------------------
// iff(test, true-part)
// iff(test, true-part, false-part)
//
define macro iff
    { iff(?test:expression, ?true:expression, ?false:expression) }
 => { if (?test) ?true else ?false end }
  { iff(?test:expression, ?true:expression) }
    => { if (?test) ?true end }
end;


// ----------------------------------------------------------------------
define macro with-restart
  { with-restart (?condition:expression, #rest ?initargs:*)
      ?:body
    end }
    => { block ()
	   ?body
	 exception (?condition, init-arguments: vector(?initargs))
	   values(#f, #t)
	 end }
end macro with-restart;

// with-simple-restart("Retry opening file") ... end
//
define macro with-simple-restart
  { with-simple-restart (?format-string:expression, ?format-args:*)
      ?:body
    end }
    => { with-restart (<simple-restart>,
		       format-string: ?format-string,
		       format-arguments: vector(?format-args))
	   ?body
         end }
end macro with-simple-restart;


// ----------------------------------------------------------------------
// define class <my-class> (<singleton-object>) ... end
//
define open abstract class <singleton-object> (<object>)
end;

// Maps classes to their singleton instances.
define constant $singletons :: <table> = make(<table>);

define method make
    (class :: subclass(<singleton-object>), #rest args, #key)
 => (object :: <singleton-object>)
  element($singletons, class, default: #f)
  | begin
      $singletons[class] := next-method()
    end
end;


// ----------------------------------------------------------------------
define macro inc!
  { inc! (?place:expression, ?dx:expression) }
    => { ?place := ?place + ?dx; }
  { inc! (?place:expression) }
    => { ?place := ?place + 1; }
end macro inc!;

define macro dec!
  { dec! (?place:expression, ?dx:expression) }
    => { ?place := ?place - ?dx; }
  { dec! (?place:expression) }
    => { ?place := ?place - 1; }
end macro dec!;


// ----------------------------------------------------------------------
// Convert a string to a floating point number.
// This version is from Chris Double's dylanlibs project, and
// seems to be the most precise of the three.  I renamed it
// from formatted-string-to-float to string-to-float.  I added
// min(..., 7) in a couple places as a quick kludge to keep from
// getting integer overflow errors.  Should figure out the right
// way...  -cgay
//
define method string-to-float(s :: <string>) => (f :: <float>)
  local method is-digit?(ch :: <character>) => (b :: <boolean>)
    let v = as(<integer>, ch);
    v >= as(<integer>, '0') & v <= as(<integer>, '9');
  end method;
  let lhs = make(<stretchy-vector>);
  let rhs = make(<stretchy-vector>);
  let state = #"start";
  let sign = 1;

  local method process-char(ch :: <character>)
    select(state)
      #"start" =>
        select(ch)
          '-' => 
            begin
              sign := -1;
              state := #"lhs";
            end;
          '+' =>
            begin
              sign := 1;
              state := #"lhs";
            end;
          '.' =>
            begin
              lhs := add!(lhs, '0');
              state := #"rhs";
            end;
          otherwise =>
            begin
              state := #"lhs";
              process-char(ch);
            end;
        end select;
      #"lhs" => 
        case
          is-digit?(ch) => lhs := add!(lhs, ch);
          ch == '.' => state := #"rhs";
          otherwise => error("Invalid floating point value.");
        end case;
      #"rhs" =>
        case
          is-digit?(ch) => rhs := add!(rhs, ch);
          otherwise => error("Invalid floating point value.");
        end case;
      otherwise => error("Invalid state while parsing floating point.");
    end select;
  end method;

  for(ch in s)
    process-char(ch);
  end for;

  let lhs = as(<string>, lhs);
  let rhs = if(empty?(rhs)) "0" else as(<string>, rhs) end;
  (string-to-integer(lhs) * sign)
   + as(<double-float>, string-to-integer(rhs) * sign)
     / (10 ^ min(rhs.size, 7)); 
end method string-to-float;

// Convert a floating point to a string without the Dylan specific formatting.
// Prints to the given number of decimal places.
// Written by Chris Double, as part of dylanlibs.
//
define method float-to-formatted-string
    (value :: <float>, #key decimal-places)
 => (s :: <string>)
  let value = iff(decimal-places,
                  as(<double-float>, truncate(value * 10 ^ min(decimal-places, 7))) / 10d0 ^ decimal-places,
                  value);
  let s = float-to-string(value);
  let dp = subsequence-position(s, ".");
  let tp = subsequence-position(s, "d") | subsequence-position(s, "s") | s.size;
  let lhs = copy-sequence(s, end: dp);
  let rhs = copy-sequence(s, start: dp + 1, end: tp);
  let shift = if(tp = s.size) 0  else string-to-integer(s, start: tp + 1) end;
  let result = "";
  let temp = concatenate(lhs, rhs);
  let d = lhs.size - 1 + shift;
  if(shift < 0)
    for(n from 0 below abs(shift))
      temp := concatenate("0", temp);
    end for;
    d := 0;
  elseif(shift > 0)
    for(n from 0 below shift)
      temp := concatenate(temp, "0");
    end for;
    d := temp.size;
  end if;
      
  let tsize = temp.size;
  concatenate(copy-sequence(temp, start: 0, end: min(d + 1, tsize)),
              iff(d = tsize, "", "."),
              iff(d = tsize,
                  "",
                  copy-sequence(temp, 
                                start: d + 1, 
                                end: iff(decimal-places,
                                         min(d + 1 + decimal-places, tsize),
                                         tsize))));
end method float-to-formatted-string;



// ----------------------------------------------------------------------
// join(range(from: 1, to: 3), ", ",
//      key: integer-to-string,
//      conjunction: " and ");
// => "1, 2 and 3"

define open generic join
    (sequence :: <sequence>, separator :: <string>,
     #key key :: <function> = identity,
          conjunction :: false-or(<string>))
 => (result :: <string>);

define method join
    (seq :: <sequence>, separator :: <string>,
     #key key :: <function> = identity,
         conjunction :: false-or(<string>))
 => (result :: <string>)
  with-output-to-string (out)
    let len-1 :: <integer> = seq.size - 1;
    for (i :: <integer> from 1,
         item in seq)
      write(out, key(item));
      if (i < len-1)
        write(out, separator);
      elseif (i == len-1)
        write(out, conjunction | separator);
      end;
    end;
  end
end;


define inline-only function whitespace?
    (ch :: <character>) => (white? :: <boolean>)
  member?(ch, #[' ', '\t', '\n'])
end;



// This should replace 'split' in common-extensions.
define open generic split
    (string :: <string>,
     #key separator :: false-or(<string>),
          start: bpos :: <integer>, 
          end: epos :: <integer>,
          trim? :: <boolean>,
          max :: false-or(<integer>),
          allow-empty-strings? :: <boolean>)
 => (strings :: <sequence>);

define method split
    (string :: <byte-string>,
     #key separator :: false-or(<byte-string>),
          start :: <integer> = 0, 
          end: _end :: <integer> = size(string),
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
  let results :: <stretchy-vector> = make(<stretchy-vector>);
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
	    add!(results, copy-sequence(string, start: start, end: _end))
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
    end
  end;
  add-substring(bpos, epos);
  results
end method splitf;

//split("1,2,,4", separator: ",", allow-empty-strings?: #t);

// ----------------------------------------------------------------------
// For removing certain keyword/value pairs from argument lists before
// passing them along with apply or next-method.
//
define method remove-keys
    (arglist :: <sequence>, #rest keys) => (x :: <list>)
  let result :: <list> = #();
  let last-pair = #f;
  for (arg in arglist, i from 0)
    if (even?(i))
      if (~ member?(arg, keys))
        if (last-pair)
          tail(last-pair) := list(arg);
        else
          result := list(arg);
          last-pair := result;
        end;
        tail(last-pair) := list(arglist[i + 1]);
        last-pair := tail(last-pair);
      end;
    end;
  end;
  result
end method remove-keys;


// ----------------------------------------------------------------------
// Seems like this should be in the core language.
//
define sideways method as
    (type == <integer>, value :: <string>) => (i :: <integer>)
  string-to-integer(value)
end;

// ----------------------------------------------------------------------
// raise(<my-error>, "%s broke", foo)

define method raise
    (class :: subclass(<condition>), format-string, #rest args)
  signal(make(class,
              format-string: format-string,
              format-arguments: copy-sequence(args)))
end;

