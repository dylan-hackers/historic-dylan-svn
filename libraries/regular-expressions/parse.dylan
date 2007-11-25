module: regular-expressions-impl
author: Nick Kramer (nkramer@cs.cmu.edu)
copyright: see below

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// This is a program to parse regular expressions. The grammar I'm using is:
//
//      <regexp> ::= <alternative> | <alternative>|<regexp>
//
//      <alternative> ::= <quantified-atom> | <quantified-atom><alternative>
//
//      <quantified-atom> ::= <atom> | <atom><quantifier>
//
//      <quantifier> ::= * | + | ? | {n} | {n,} | {n, m}
//            (where n and m are decimal integers)
//
//      <atom> ::= (<regexp>) | <extended-character>
//
// See "Programming perl", p. 103-104 for more details.
//
// Because an assertion is a type of <extended-character>, this will
// parse a "quantified assertion", which really isn't a legal regular
// expression component.  Match.dylan could go into an infinite loop
// if given this.

define abstract class <parsed-regexp> (<object>)
end class <parsed-regexp>;

// The root of the parsed regexp, i.e., this is what's returned by the parser.
define class <regexp> (<mark>)
  constant slot regexp-pattern :: <string>, required-init-keyword: #"pattern";
  constant slot regexp-group-count :: <integer>, required-init-keyword: #"group-count";
end class <regexp>;

define class <mark> (<parsed-regexp>)
  slot child :: <parsed-regexp>,  required-init-keyword: #"child";
  constant slot group-number :: <integer>, required-init-keyword: #"group";
end class <mark>;

define class <union> (<parsed-regexp>)          //    |
  slot left  :: <parsed-regexp>, required-init-keyword: #"left";
  slot right :: <parsed-regexp>, required-init-keyword: #"right";
end class <union>;

define class <alternative> (<parsed-regexp>)    // concatenation
  slot left :: <parsed-regexp>,  required-init-keyword: #"left";
  slot right :: <parsed-regexp>, required-init-keyword: #"right";
end class <alternative>;

define class <parsed-assertion> (<parsed-regexp>)
  constant slot asserts :: <symbol>, required-init-keyword: #"assertion";
end class <parsed-assertion>;

define class <quantified-atom> (<parsed-regexp>)
  slot atom :: <parsed-regexp>, required-init-keyword: #"atom";
  constant slot min-matches :: <integer>, init-value: 0,
    init-keyword: #"min";
  constant slot max-matches :: false-or(<integer>), init-value: #f, 
    init-keyword: #"max";
end class <quantified-atom>;

define abstract class <parsed-atom> (<parsed-regexp>)
end class <parsed-atom>;

define class <parsed-character> (<parsed-atom>)
  constant slot character :: <character>, required-init-keyword: #"character";
end class <parsed-character>;

define class <parsed-string> (<parsed-atom>)
  constant slot string :: <string>, required-init-keyword: #"string";
end class <parsed-string>;

define class <parsed-set> (<parsed-atom>)
  constant slot char-set :: <character-set>, required-init-keyword: #"set";
end class <parsed-set>;

define class <parsed-backreference> (<parsed-atom>)
  constant slot group-number :: <integer>, required-init-keyword: #"group"; 
end class <parsed-backreference>;

// Note: I'm pretty sure <simple-error> won't work in GD.  --cgay
define class <regexp-error> (<simple-error>)
end class <regexp-error>;

define class <illegal-regexp> (<regexp-error>)
  constant slot regexp-pattern :: <string>, 
    required-init-keyword: #"pattern";
end class <illegal-regexp>;

define sealed domain make (singleton(<illegal-regexp>));
define sealed domain initialize (<illegal-regexp>);

define function parse-error
    (pattern :: <string>, format-string :: <string>, #rest format-args)
  let msg = apply(format-to-string, format-string, format-args);
  signal(make(<illegal-regexp>,
              format-string: "Invalid regular expression: %=.  %s",
              format-arguments: list(pattern, msg),
              pattern: pattern));
end function parse-error;

// <parse-info> contains some information about the current regexp
// being parsed.  Using a structure is slightly nicer than having
// global variables..
//
define class <parse-info> (<object>)
  // Name this has-backreferences, for consistency with the other slots?
  // Add ? to all the has-* slots.  --cgay
  slot backreference-used :: <boolean>, init-value: #f;
     // Whether or not the function includes \1, \2, etc in the regexp.
     // This is different from return-marks, which determines whether the
     // user wants to know about the marks.
  slot has-alternatives :: <boolean>, init-value: #f;
  slot has-quantifiers :: <boolean>, init-value: #f;
  slot current-group-number :: <integer>, init-value: 0;
  constant slot set-type :: <class>, required-init-keyword: #"set-type";
end class <parse-info>;

define function make-parse-info
    (#key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f)
 => (info :: <parse-info>)
  local method nyi (option-name)
          signal(make(<regexp-error>,
                      format-string: "The '%s' option is not yet implemented.",
                      format-arguments: list(option-name)));
        end;
  verbose & nyi("verbose");
  multi-line & nyi("multi-line");
  dot-matches-all & nyi("dot-matches-all");
  let char-set-type
   = if (case-sensitive)
       <case-sensitive-character-set>
     else
       <case-insensitive-character-set>
     end;
  make(<parse-info>, set-type: char-set-type)
end function make-parse-info;

define method parse
    (regexp :: <string>, parse-info :: <parse-info>)
 => (parsed-regexp :: <parsed-regexp>,
     last-group :: <integer>,
     backrefs? :: <boolean>,
     alternatives? :: <boolean>, 
     quantifiers? :: <boolean>)
  let parse-string = make(<parse-string>, string: regexp);
  let child = parse-regexp(parse-string, parse-info);
  let parse-tree = make(<regexp>,
                        pattern: regexp,
                        group-count: parse-info.current-group-number + 1,
                        group: 0,
			child: child);
  let optimized-regexp = optimize(parse-tree);
  if (optimized-regexp.pathological?)
    parse-error(regexp, "A sub-regexp that matches the empty string was quantified.");
  else
    values(optimized-regexp,
	   parse-info.current-group-number,
	   parse-info.backreference-used,
	   parse-info.has-alternatives,
	   parse-info.has-quantifiers);
  end if;
end method parse;

define method parse-regexp (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let alternative = parse-alternative(s, info);
  if (~alternative)
    parse-error(s.parse-string, "");
  elseif (lookahead(s) = '|')
    info.has-alternatives := #t;
    make(<union>, left: alternative, right: parse-regexp(consume(s), info));
  else
    alternative;
  end if;
end method parse-regexp;

define method parse-alternative
    (s :: <parse-string>, info :: <parse-info>)
 => (re :: false-or(<parsed-regexp>))
  let term = parse-quantified-atom(s, info);
  if (member?(lookahead(s), #(#f, '|', ')')))
    term;
  else
    make(<alternative>, left: term, right: parse-alternative(s, info));
  end if;
end method parse-alternative;

define method parse-quantified-atom (s :: <parse-string>, info :: <parse-info>)
 => (result :: false-or(<parsed-regexp>))
  let atom = parse-atom(s, info);
  let char = lookahead(s);
  select (char by \=)
    '*' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 0, atom: atom);

    '+' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 1, atom: atom);

    '?' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 0, max: 1, atom: atom);

    '{' =>
      info.has-quantifiers := #t;
      consume(s);
      parse-minmax-quantifier(atom, s);

    otherwise =>
      atom;
  end select;
end method parse-quantified-atom;

// {m,n}, {m,}, {,n}, {m}, {}, and {,} are all valid.
// m defaults to 0 and n defaults to #f (unlimited).
define method parse-minmax-quantifier
    (atom :: <parsed-regexp>, s :: <parse-string>) => (qatom :: <quantified-atom>)
  local method parse-integer () => (int :: false-or(<integer>))
          let digits = make(<deque>);
          while (lookahead(s) & digit?(lookahead(s)))
            push-last(digits, lookahead(s));
            consume(s);
          end;
          ~empty?(digits) & string-to-integer(as(<byte-string>, digits));
        end method parse-integer;
  let qmin = parse-integer() | 0;
  let qmax = #f;
  if (lookahead(s) = ',')
    consume(s);
    qmax := parse-integer();
  else
    qmax := qmin;
  end;
  if (lookahead(s) ~= '}')
    parse-error(s.parse-string,
                "Close brace expected in {m,n} quantifier (index = %s).",
                s.parse-index);
  end;
  consume(s);
  make(<quantified-atom>, atom: atom, min: qmin, max: qmax);
end method parse-minmax-quantifier;

define method parse-atom (s :: <parse-string>, info :: <parse-info>)
 => (regexp :: false-or(<parsed-regexp>))
  let char = lookahead(s);
  select (char)
    '(' =>
      consume(s);   // Consume beginning paren
      parse-group(s, info);

    ')' =>
      #f;              // Need something to terminate upon seeing a close paren

    #f  =>
      #f;   // Signal error?  (end of stream)

    '*', '|', '+' =>
      #f;

    '\\' =>
      consume(s);        // Consume the backslash
      // Perhaps add support for a different escape character to aid readability.
      // The escape character could be specified in the <parse-info>.  --cgay
      parse-escaped-character(s, info);

    '[' =>
      consume(s);        // Eat the opening brace
      parse-character-set(s, info);

    '.' =>
      consume(s);
      dot;

    '^' =>
      consume(s);
      make(<parsed-assertion>, assertion: #"beginning-of-string");

    '$' =>
      consume(s);
      make(<parsed-assertion>, assertion: #"end-of-string");
  
      // Insert more special characters here

    otherwise =>
      let char = lookahead(s);
      consume(s);
      make(<parsed-character>, character: char);
  end select;
end method parse-atom;

define inline function parse-group
    (s :: <parse-string>, info :: <parse-info>)
 => (mark :: <mark>)
  info.current-group-number := info.current-group-number + 1;
  let this-group = info.current-group-number;
  let regexp = parse-regexp(s, info);
  if (lookahead(s) = ')')
    consume(s);   // Consume ')'
    make(<mark>, child: regexp, group: this-group)
  else
    parse-error(s.parse-string, "Unbalanced parens in regexp (index = %s).",
                s.parse-index);
  end;
end function parse-group;

// This just does a quick scan to find the closing ] and then lets
// make(<character-set>) do the real parsing.
//
define inline function parse-character-set
    (s :: <parse-string>, info :: <parse-info>)
 => (set :: <parsed-set>)
  let set-string = make(<deque>);      // Need something that'll 
                                       // preserve the right ordering
  let start-index = s.parse-index;
  for (char = lookahead(s) then lookahead(s),
       until: char == ']')
    consume(s);                    // eat char
    if (~char)
      parse-error(s.parse-string,
                  "Unterminated character set at index %d.",start-index);
    elseif (char ~== '\\')
      push-last(set-string, char);
    else
      let char2 = lookahead(s);
      consume(s);  // Eat escaped char
      if (char2 == ']')
        push-last(set-string, ']');
      else
        push-last(set-string, '\\');
        push-last(set-string, char2);
      end if;
    end if;
  end for;
  consume(s);     // Eat ending brace
  make(<parsed-set>, set: make(info.set-type, description: set-string));
end function parse-character-set;

define constant any-char 
  = make(<case-sensitive-character-set>, description: "^\n");

// The useful definitions of all these are in as(<character-set>).
//
define constant digit-chars
  = make(<case-sensitive-character-set>, description: "\\d");
define constant not-digit-chars
  = make(<case-sensitive-character-set>, description: "^\\d");
define constant word-chars
  = make(<case-sensitive-character-set>, description: "\\w");
define constant not-word-chars
  = make(<case-sensitive-character-set>, description: "^\\w");
define constant whitespace-chars
  = make(<case-sensitive-character-set>, description: "\\s");
define constant not-whitespace-chars
  = make(<case-sensitive-character-set>, description: "^\\s");

define constant dot = make(<parsed-set>, set: any-char);
/* KJP: Not used.
define constant dot-star = make(<quantified-atom>, min: 0, max: #f,
				atom: dot);
*/

// This only handles escaped characters *outside* of a character
// set. Inside of a character set is a whole different story.
//
define method parse-escaped-character 
    (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let next-char = lookahead(s);
  if (~next-char)
    parse-error(s.parse-string,
                "Unterminated escape sequence at index %d.",
                s.parse-index - 1)
  end;
  consume(s);
  select (next-char)
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' =>
      info.backreference-used := #t;
      make(<parsed-backreference>, group: digit-to-integer(next-char));
      
    // Hmm.  Why would you write \\n in your regex instead of \n?  It has the
    // same effect.  Also, what about the rest of the Dylan character escapes?
    // --cgay
    'n' =>   make(<parsed-character>, character: '\n');   // Newline
    't' =>   make(<parsed-character>, character: '\t');   // Tab
    'f' =>   make(<parsed-character>, character: '\f');   // Formfeed
    'r' =>   make(<parsed-character>, character: '\r');   // Carriage return

    'b' =>   make(<parsed-assertion>, assertion: #"word-boundary");
    'B' =>   make(<parsed-assertion>, assertion: #"not-word-boundary");
       // Beginning and end of string are not escaped

    'd' =>   make(<parsed-set>, set: digit-chars);
    'D' =>   make(<parsed-set>, set: not-digit-chars);
    'w' =>   make(<parsed-set>, set: word-chars);
    'W' =>   make(<parsed-set>, set: not-word-chars);
    's' =>   make(<parsed-set>, set: whitespace-chars);
    'S' =>   make(<parsed-set>, set: not-whitespace-chars);

    // Insert more escaped characters here

    otherwise =>
      make(<parsed-character>, character: next-char);
  end select;
end method parse-escaped-character;

define method is-anchored? (regexp :: <parsed-regexp>)
 => (result :: <boolean>);
  select (regexp by instance?)
    <mark> => is-anchored?(regexp.child);
    <alternative> => is-anchored?(regexp.left);
    <parsed-assertion> => regexp.asserts == #"beginning-of-string";
    otherwise => #f;
  end select;
end method is-anchored?;

define method initial-substring (regexp :: <parsed-regexp>)
 => (result :: <string>);
  let result = make(<deque>);
  local method init (regexp :: <parsed-regexp>, result :: <deque>)
	  select (regexp by instance?)
	    <alternative> =>
	      init(regexp.left, result) & init(regexp.right, result);
	    <parsed-character> =>
	      push-last(result, regexp.character);
	    <parsed-string> =>
	      for (ch in regexp.string) push-last(result, ch) end for;
	    <mark> =>
	      init(regexp.child, result);
	    <parsed-assertion> =>
	      #t;
	    otherwise =>
	      #f;
	  end select;
	end method init;
  init(regexp, result);
  as(<byte-string>, result);
end method initial-substring;

// Optimize converts a parse tree into an "optimized" parse tree.
// Currently the only optimization is merging adjacent characters into
// a string.
//
define method optimize (regexp :: <parsed-regexp>)
 => (regexp :: <parsed-regexp>);
  select (regexp by instance?)
    <mark> =>
      regexp.child := optimize(regexp.child);
      regexp;
    <alternative> =>
      if (instance?(regexp.left, <parsed-character>))
	let result-str = make(<deque>);
	push-last(result-str, regexp.left.character);
	for (next = regexp.right then next.right,
	     while: (instance?(next, <alternative>)
		       & instance?(next.left, <parsed-character>)))
	  push-last(result-str, next.left.character)
	finally
	  if (instance?(next, <parsed-character>))
	    push-last(result-str, next.character);
	    make(<parsed-string>, string: as(<string>, result-str));
	  elseif (result-str.size = 1)
	    regexp.right := optimize(regexp.right);
	    regexp;
	  else
	    make(<alternative>,
		 left: make(<parsed-string>, string: as(<string>, result-str)),
		 right: optimize(next));
	  end if;
	end for;
      else
	regexp.left := optimize(regexp.left);
	regexp.right := optimize(regexp.right);
	regexp;
      end if;
    <union> =>
      regexp.left := optimize(regexp.left);
      regexp.right := optimize(regexp.right);
      regexp;
    <quantified-atom> =>
      regexp.atom := optimize(regexp.atom);
      regexp;
    otherwise =>
      regexp;
  end select;
end method optimize;

// We have to somehow deal with pathological regular expressions like
// ".**".  Perl simply signals an error in this case.  We *could* in
// fact match these pathological regexps using the formulation below,
// but it doesn't seem worth the trouble.  Frankly, I doubt anyone has
// ever tried to use such a pathological regexp and *not* have done it
// by mistake.  But in case I'm wrong, here's how to fix a
// pathological regexp:
//
// First, realize that pathological regexps stem from infinitely
// quantifying subregexps that could match the empty string.  So what
// we do is find this subregexps, and perform the following
// transformation:
//
//  case (type of regexp)
//    r1r2 => r1'r2|r2'
//    r1|r2 => r1'|r2'
//    r1{0,n} => r1'{1,n}
//    r1{0,} => r1'{1,}
//    atom => atom
//    assertion => can't be done
//
// This transformation turns a might-match-emptystring regexp into a
// regexp that matches the same set of strings minus the empty string.
// If this transformation can't be done, remember that "$*" is
// equivalent to "always true and consumes no input".


define generic matches-empty-string? (regexp :: <parsed-regexp>)
 => answer :: <boolean>;

define method matches-empty-string? (regexp :: <parsed-atom>)
 => answer :: <boolean>;
  #f;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <parsed-assertion>)
 => answer :: <boolean>;
  #t;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <mark>)
 => answer :: <boolean>;
  regexp.child.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <union>)
 => answer :: <boolean>;
  regexp.left.matches-empty-string? | regexp.right.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <alternative>)
 => answer :: <boolean>;
  regexp.left.matches-empty-string? & regexp.right.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regexp :: <quantified-atom>)
 => answer :: <boolean>;
   regexp.min-matches == 0 | regexp.atom.matches-empty-string?;
end method matches-empty-string?;


define generic pathological? (regexp :: <parsed-regexp>)
 => answer :: <boolean>;

define method pathological? (regexp :: <parsed-atom>)
 => answer :: <boolean>;
  #f;
end method pathological?;

define method pathological? (regexp :: <parsed-assertion>)
 => answer :: <boolean>;
  #f;
end method pathological?;

define method pathological? (regexp :: <mark>)
 => answer :: <boolean>;
  regexp.child.pathological?;
end method pathological?;

define method pathological? (regexp :: <union>)
 => answer :: <boolean>;
  regexp.left.pathological? | regexp.right.pathological?;
end method pathological?;

define method pathological? (regexp :: <alternative>)
 => answer :: <boolean>;
  regexp.left.pathological? | regexp.right.pathological?;
end method pathological?;

define method pathological? (regexp :: <quantified-atom>)
 => answer :: <boolean>;
  regexp.max-matches == #f & regexp.atom.matches-empty-string?;
end method pathological?;

// Seals for file parse.dylan

// <mark> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<mark>));
// <union> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<union>));
// <alternative> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<alternative>));
// <parsed-assertion> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<parsed-assertion>));
// <quantified-atom> -- subclass of <parsed-regexp>
define sealed domain make(singleton(<quantified-atom>));
// <parsed-character> -- subclass of <parsed-atom>
define sealed domain make(singleton(<parsed-character>));
// <parsed-string> -- subclass of <parsed-atom>
define sealed domain make(singleton(<parsed-string>));
// <parsed-set> -- subclass of <parsed-atom>
define sealed domain make(singleton(<parsed-set>));
// <parsed-backreference> -- subclass of <parsed-atom>
define sealed domain make(singleton(<parsed-backreference>));
// <parse-info> -- subclass of <object>
define sealed domain make(singleton(<parse-info>));
define sealed domain initialize(<parse-info>);
