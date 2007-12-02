module: regular-expressions
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
//      <regex> ::= <alternative> | <alternative>|<regex>
//
//      <alternative> ::= <quantified-atom> | <quantified-atom><alternative>
//
//      <quantified-atom> ::= <atom> | <atom><quantifier>
//
//      <quantifier> ::= * | + | ? | {n} | {n,} | {n, m}
//            (where n and m are decimal integers)
//
//      <atom> ::= <subpattern> | <extended-character>
//
//      <subpattern> ::= (<options> <regex>)
//
//      <options> ::= ?: | ?P<name> | ?P=name | ?# | etc
//
// See "Programming perl", p. 103-104 for more details.
//
// Because an assertion is a type of <extended-character>, this will
// parse a "quantified assertion", which really isn't a legal regular
// expression component.  Match.dylan could go into an infinite loop
// if given this.

define abstract class <parsed-regex> (<object>)
end class <parsed-regex>;

define class <mark> (<parsed-regex>)
  slot child :: <parsed-regex>,  required-init-keyword: #"child";
  constant slot group-number :: <integer>, required-init-keyword: #"group";
end class <mark>;

// The root of the parsed regex, i.e., this is what's returned by the parser.
define class <regex> (<mark>)
  // exported
  constant slot regex-pattern :: <string>,
    required-init-keyword: pattern:;
  // exported
  constant slot regex-group-count :: <integer>,
    required-init-keyword: group-count:;
  // internal.  This is only needed when making a <regex-match> after
  // a successful search.
  constant slot group-number-to-name :: <table>,
    required-init-keyword: group-number-to-name:;
end class <regex>;

define class <union> (<parsed-regex>)          //    |
  slot left  :: <parsed-regex>, required-init-keyword: #"left";
  slot right :: <parsed-regex>, required-init-keyword: #"right";
end class <union>;

define class <alternative> (<parsed-regex>)    // concatenation
  slot left :: <parsed-regex>,  required-init-keyword: #"left";
  slot right :: <parsed-regex>, required-init-keyword: #"right";
end class <alternative>;

define class <parsed-assertion> (<parsed-regex>)
  constant slot asserts :: <symbol>, required-init-keyword: #"assertion";
end class <parsed-assertion>;

define class <quantified-atom> (<parsed-regex>)
  slot atom :: <parsed-regex>, required-init-keyword: #"atom";
  constant slot min-matches :: <integer>, init-value: 0,
    init-keyword: #"min";
  constant slot max-matches :: false-or(<integer>), init-value: #f, 
    init-keyword: #"max";
end class <quantified-atom>;

define abstract class <parsed-atom> (<parsed-regex>)
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
define class <regex-error> (<simple-error>)
end class <regex-error>;

define class <illegal-regex> (<regex-error>)
  constant slot regex-pattern :: <string>, 
    required-init-keyword: #"pattern";
end class <illegal-regex>;

define sealed domain make (singleton(<illegal-regex>));
define sealed domain initialize (<illegal-regex>);

define function parse-error
    (pattern :: <string>, format-string :: <string>, #rest format-args)
  let msg = apply(format-to-string, format-string, format-args);
  signal(make(<illegal-regex>,
              format-string: "Invalid regular expression: %=.  %s",
              format-arguments: list(pattern, msg),
              pattern: pattern));
end function parse-error;

define function not-yet-implemented (thing, #rest format-args)
  let thing = if (~empty?(format-args))
                apply(format-to-string, thing, format-args)
              else
                thing
              end;
  signal(make(<regex-error>,
              format-string: "The %s is not yet implemented.",
              format-arguments: list(thing)));
end;

// <parse-info> contains some information about the current regex
// being parsed.
//
define class <parse-info> (<object>)
  // Whether or not the function includes \1, \2, etc in the regex.
  // Name this has-backreferences, for consistency with the other slots.
  // Add ? to all the has-* slots.  --cgay
  // Also, not sure why anyone cares about these three things.
  slot backreference-used :: <boolean>, init-value: #f;
  slot has-alternatives :: <boolean>, init-value: #f;
  slot has-quantifiers :: <boolean>, init-value: #f;
  slot current-group-number :: <integer>, init-value: 0;
  constant slot group-number-to-name :: <table>, init-value: make(<table>);
  constant slot set-type :: <class>, required-init-keyword: #"set-type";
end class <parse-info>;

define function make-parse-info
    (#key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f)
 => (info :: <parse-info>)
  verbose & not-yet-implemented("'verbose' option");
  multi-line & not-yet-implemented("'multi-line' option");
  dot-matches-all & not-yet-implemented("'dot-matches-all' option");
  let char-set-type
   = if (case-sensitive)
       <case-sensitive-character-set>
     else
       <case-insensitive-character-set>
     end;
  make(<parse-info>, set-type: char-set-type)
end function make-parse-info;

define method has-named-group?
    (info :: <parse-info>, name :: <string>)
  member?(name, info.group-number-to-name, test: \=)
end;

define method parse
    (regex :: <string>, parse-info :: <parse-info>)
 => (parsed-regex :: <parsed-regex>,
     last-group :: <integer>,
     backrefs? :: <boolean>,
     alternatives? :: <boolean>, 
     quantifiers? :: <boolean>)
  let parse-string = make(<parse-string>, string: regex);
  let child = parse-regex(parse-string, parse-info);
  let parse-tree = make(<regex>,
                        pattern: regex,
                        group: 0,
                        group-count: parse-info.current-group-number + 1,
                        group-number-to-name: parse-info.group-number-to-name,
			child: child);
  let optimized-regex = optimize(parse-tree);
  if (optimized-regex.pathological?)
    parse-error(regex, "A subpattern that matches the empty string was quantified.");
  else
    values(optimized-regex,
	   parse-info.current-group-number,
	   parse-info.backreference-used,
	   parse-info.has-alternatives,
	   parse-info.has-quantifiers);
  end if;
end method parse;

define method parse-regex (s :: <parse-string>, info :: <parse-info>)
 => parsed-regex :: <parsed-regex>;
  let alternative = parse-alternative(s, info);
  if (~alternative)
    parse-error(s.parse-string, "");
  elseif (lookahead(s) = '|')
    info.has-alternatives := #t;
    make(<union>, left: alternative, right: parse-regex(consume(s), info));
  else
    alternative;
  end if;
end method parse-regex;

define method parse-alternative
    (s :: <parse-string>, info :: <parse-info>)
 => (re :: false-or(<parsed-regex>))
  let term = parse-quantified-atom(s, info);
  if (member?(lookahead(s), #(#f, '|', ')')))
    term;
  else
    make(<alternative>, left: term, right: parse-alternative(s, info));
  end if;
end method parse-alternative;

define method parse-quantified-atom (s :: <parse-string>, info :: <parse-info>)
 => (result :: false-or(<parsed-regex>))
  // I think this breaks when parse-atom returns #f and then we quantify that.
  // I added some regexes to regression-tests.txt starting with /a()b/ that I
  // hope will exercise that case.  --cgay
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
    (atom :: <parsed-regex>, s :: <parse-string>) => (qatom :: <quantified-atom>)
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
 => (regex :: false-or(<parsed-regex>))
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
      consume(s);
      make(<parsed-character>, character: char);
  end select;
end method parse-atom;

// Parse a subpattern, a.k.a. "group".  i.e., something delimited by parens.
// The parse string is pointing at the character after the '('.
//
define inline function parse-group
    (str :: <parse-string>, info :: <parse-info>)
 => (mark :: false-or(<parsed-regex>))
  let char = lookahead(str);
  if (char == '?')
    consume(str);
    parse-extended-group(str, info)
  else
    parse-simple-group(str, info, #t, #f)
  end
end function parse-group;

// Just saw "(?" so we need to parse a group with extended options.
//
define inline function parse-extended-group
    (str :: <parse-string>, info :: <parse-info>)
 => (mark :: false-or(<parsed-regex>))
  let char = lookahead(str);
  consume(str);
  select (char)
    'P' =>                    // (?P named group constructs
      let char = lookahead(str);
      consume(str);
      if (char == '=')
        not-yet-implemented("(?P=name) construct");
      elseif (char = '<')
        parse-simple-group(str, info, #t, parse-group-name(str, info))
      else
        parse-error(str.parse-string,
                    "Invalid named group syntax (index = %s).",
                    str.parse-index);
      end;

    ':' =>                    // (?: doesn't save the group
      parse-simple-group(str, info, #f, #f);

    '#' =>                    // (?# for comments
      while (lookahead(str) & lookahead(str) ~== ')')
        consume(str);
      end;
      if (~ lookahead(str))
        parse-error(str.parse-string, "Unterminated subpattern commend (?#....");
      else
        #f
      end;

    otherwise =>
      // See the Python re docs for what all these do.
      if (member?(char, "iLmsux=!<("))
        not-yet-implemented("'(?%c' subpattern construct", char);
      else
        parse-error(str.parse-string, "Invalid subpattern construct (?%c...) at index %s.",
                    char, str.parse-index);
      end;
  end select
end function parse-extended-group;

// Just saw "(?P<", so parse the name of this named group.
//
define function parse-group-name
    (str :: <parse-string>, info :: <parse-info>)
 => (name :: <string>)
  let start-index = str.parse-index;
  while (lookahead(str) & lookahead(str) ~== '>')
    consume(str);
  end;
  if (lookahead(str) == '>')
    consume(str);
    copy-sequence(str.parse-string, start: start-index, end: str.parse-index - 1)
  else
    parse-error(str.parse-string, "Unterminated named group name at index %s.",
                str.parse-index);
  end
end function parse-group-name;

// Parse a group/subpattern, possibly after having already parsed any
// options given via "(?...".
//
define inline function parse-simple-group
    (str :: <parse-string>,
     info :: <parse-info>,
     save-group? :: <boolean>,
     group-name :: false-or(<string>))
 => (mark :: false-or(<parsed-regex>))
  if (save-group?)
    info.current-group-number := info.current-group-number + 1;
  end;
  let regex = parse-regex(str, info);
  if (lookahead(str) ~== ')')
    parse-error(str.parse-string, "Unbalanced parens in regex (index = %s).",
                str.parse-index);
  else
    consume(str);
    if (~ save-group?)
      regex
    else
      if (group-name)
        if (has-named-group?(info, group-name))
          parse-error(str.parse-string,
                      "Duplicate group name (%s) at index %s.",
                      group-name, str.parse-index);
        else
          info.group-number-to-name[info.current-group-number] := group-name;
        end;
      end;
      make(<mark>, child: regex, group: info.current-group-number)
    end
  end
end function parse-simple-group;

// This just does a quick scan to find the closing ] and then lets
// make(<character-set>) do the real parsing.
//
define inline function parse-character-set
    (str :: <parse-string>, info :: <parse-info>)
 => (set :: <parsed-set>)
  let set-string = make(<deque>);
  let start-index = str.parse-index;
  local method peek ()
          lookahead(str)
            | parse-error(str.parse-string,
                          "Unterminated character set starting at at index %d.",
                          start-index);
        end;
  block (done)
    for (char = peek() then peek(),
         charset-index from 0)
      consume(str);
      select (char)
        ']' =>
          if (charset-index == 0)
            push-last(set-string, char);  // e.g., []] is the set containing ']'
          else
            done();
          end;
        '^' =>
          push-last(set-string, '^');
          if (peek() == ']')
            consume(str);
            push-last(set-string, ']');  // e.g., [^]] is the set without ']'
          end;
        '\\' =>
          let char2 = peek();
          consume(str);  // Eat escaped char
          if (char2 == ']')
            push-last(set-string, ']');
          else
            push-last(set-string, '\\');
            push-last(set-string, char2);
          end if;
        otherwise =>
          push-last(set-string, char);
      end select
    end for;
  end block;
  make(<parsed-set>, set: make(info.set-type, description: set-string))
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
 => parsed-regex :: <parsed-regex>;
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

define method is-anchored? (regex :: <parsed-regex>)
 => (result :: <boolean>);
  select (regex by instance?)
    <mark> => is-anchored?(regex.child);
    <alternative> => is-anchored?(regex.left);
    <parsed-assertion> => regex.asserts == #"beginning-of-string";
    otherwise => #f;
  end select;
end method is-anchored?;

define method initial-substring (regex :: <parsed-regex>)
 => (result :: <string>);
  let result = make(<deque>);
  local method init (regex :: <parsed-regex>, result :: <deque>)
	  select (regex by instance?)
	    <alternative> =>
	      init(regex.left, result) & init(regex.right, result);
	    <parsed-character> =>
	      push-last(result, regex.character);
	    <parsed-string> =>
	      for (ch in regex.string) push-last(result, ch) end for;
	    <mark> =>
	      init(regex.child, result);
	    <parsed-assertion> =>
	      #t;
	    otherwise =>
	      #f;
	  end select;
	end method init;
  init(regex, result);
  as(<byte-string>, result);
end method initial-substring;

// Optimize converts a parse tree into an "optimized" parse tree.
// Currently the only optimization is merging adjacent characters into
// a string.
//
define method optimize (regex :: <parsed-regex>)
 => (regex :: <parsed-regex>);
  select (regex by instance?)
    <mark> =>
      regex.child := optimize(regex.child);
      regex;
    <alternative> =>
      if (instance?(regex.left, <parsed-character>))
	let result-str = make(<deque>);
	push-last(result-str, regex.left.character);
	for (next = regex.right then next.right,
	     while: (instance?(next, <alternative>)
		       & instance?(next.left, <parsed-character>)))
	  push-last(result-str, next.left.character)
	finally
	  if (instance?(next, <parsed-character>))
	    push-last(result-str, next.character);
	    make(<parsed-string>, string: as(<string>, result-str));
	  elseif (result-str.size = 1)
	    regex.right := optimize(regex.right);
	    regex;
	  else
	    make(<alternative>,
		 left: make(<parsed-string>, string: as(<string>, result-str)),
		 right: optimize(next));
	  end if;
	end for;
      else
	regex.left := optimize(regex.left);
	regex.right := optimize(regex.right);
	regex;
      end if;
    <union> =>
      regex.left := optimize(regex.left);
      regex.right := optimize(regex.right);
      regex;
    <quantified-atom> =>
      regex.atom := optimize(regex.atom);
      regex;
    otherwise =>
      regex;
  end select;
end method optimize;

// We have to somehow deal with pathological regular expressions like
// ".**".  Perl simply signals an error in this case.  We *could* in
// fact match these pathological regexs using the formulation below,
// but it doesn't seem worth the trouble.  Frankly, I doubt anyone has
// ever tried to use such a pathological regex and *not* have done it
// by mistake.  But in case I'm wrong, here's how to fix a
// pathological regex:
//
// First, realize that pathological regexs stem from infinitely
// quantifying subpatterns that could match the empty string.  So what
// we do is find this subpattern, and perform the following
// transformation:
//
//  case (type of regex)
//    r1r2 => r1'r2|r2'
//    r1|r2 => r1'|r2'
//    r1{0,n} => r1'{1,n}
//    r1{0,} => r1'{1,}
//    atom => atom
//    assertion => can't be done
//
// This transformation turns a might-match-emptystring regex into a
// regex that matches the same set of strings minus the empty string.
// If this transformation can't be done, remember that "$*" is
// equivalent to "always true and consumes no input".


define generic matches-empty-string? (regex :: <parsed-regex>)
 => answer :: <boolean>;

define method matches-empty-string? (regex :: <parsed-atom>)
 => answer :: <boolean>;
  #f;
end method matches-empty-string?;

define method matches-empty-string? (regex :: <parsed-assertion>)
 => answer :: <boolean>;
  #t;
end method matches-empty-string?;

define method matches-empty-string? (regex :: <mark>)
 => answer :: <boolean>;
  regex.child.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regex :: <union>)
 => answer :: <boolean>;
  regex.left.matches-empty-string? | regex.right.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regex :: <alternative>)
 => answer :: <boolean>;
  regex.left.matches-empty-string? & regex.right.matches-empty-string?;
end method matches-empty-string?;

define method matches-empty-string? (regex :: <quantified-atom>)
 => answer :: <boolean>;
   regex.min-matches == 0 | regex.atom.matches-empty-string?;
end method matches-empty-string?;


define generic pathological? (regex :: <parsed-regex>)
 => answer :: <boolean>;

define method pathological? (regex :: <parsed-atom>)
 => answer :: <boolean>;
  #f;
end method pathological?;

define method pathological? (regex :: <parsed-assertion>)
 => answer :: <boolean>;
  #f;
end method pathological?;

define method pathological? (regex :: <mark>)
 => answer :: <boolean>;
  regex.child.pathological?;
end method pathological?;

define method pathological? (regex :: <union>)
 => answer :: <boolean>;
  regex.left.pathological? | regex.right.pathological?;
end method pathological?;

define method pathological? (regex :: <alternative>)
 => answer :: <boolean>;
  regex.left.pathological? | regex.right.pathological?;
end method pathological?;

define method pathological? (regex :: <quantified-atom>)
 => answer :: <boolean>;
  regex.max-matches == #f & regex.atom.matches-empty-string?;
end method pathological?;

// Seals for file parse.dylan

// <mark> -- subclass of <parsed-regex>
define sealed domain make(singleton(<mark>));
// <union> -- subclass of <parsed-regex>
define sealed domain make(singleton(<union>));
// <alternative> -- subclass of <parsed-regex>
define sealed domain make(singleton(<alternative>));
// <parsed-assertion> -- subclass of <parsed-regex>
define sealed domain make(singleton(<parsed-assertion>));
// <quantified-atom> -- subclass of <parsed-regex>
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
