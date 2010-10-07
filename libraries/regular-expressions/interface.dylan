module:   regex-implementation
author:   Nick Kramer (nkramer@cs.cmu.edu)
          Carl Gay (changed everything except regex-position)
synopsis: The regular-expressions API, insofar as it can be separated into one file.
copyright: see below

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998-2008  Gwydion Dylan Maintainers
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

//// Caching

// Parsing a regex is not cheap, so we cache the parsed regexs and
// only parse a string if we haven't seen it before.  Because in
// practice almost all regex strings are string literals, we're free
// to choose == or = depending on whatever's fastest.  However,
// because a string is parsed differently depending on the arguments
// passed to compile-regex, we also have to keep track of that
// information as well.
//

define class <regex-cache> (<table>) end;

define method table-protocol
    (table :: <regex-cache>)
 => (equal? :: <function>, hash :: <function>)
 local method hash (key :: <list>, initial-state)
                => (id :: <integer>, state)
         let (id, state) = string-hash(head(key), initial-state);
         for (boolean in tail(key))
           let (next-id, next-state) = object-hash(boolean, state);
           id := merge-hash-ids(id, next-id, ordered: #t);
           state := next-state;
         end;
         values(id, state)
       end;
  values(\=, hash)
end method table-protocol;

// Technically not thread safe, but does it matter?  Worst case seems to
// be a duplicated regex parse.  --cgay
//
define constant *regex-cache* = make(<regex-cache>);

// Compile the given string into an optimized regular expression.
//
// @param case-sensitive -- Whether to be case sensitive when matching character
//   sets (e.g., [a-z]).  This does not affect other character/string matching yet.
//   TODO -- but it should
//
// @param verbose -- If true, allows you to write regular expressions that
//   are easier to read by including whitespace and comments in them that
//   will be ignored.
//
// @param multi-line -- If true, '^' matches at the beginning of the string and
//   at the beginning of each line (immediately following each newline); and '$'
//   matches at the end of the string and at the end of each line (immediately
//   preceding each newline). By default, "^" matches only at the beginning of
//   the string, and "$" only at the end of the string.
//
// @param dot-matches-all -- Normally '.' matches any character except for
//   newline.  If this parameter is true '.' matches newline as well.
//
// @param use-cache -- If true then check for a regex in the cache matching
//   the given set of arguments.  If not found in the cache, compile it and
//   then add it to the cache (and return it).
//
// This function signals <invalid-regex> if the regular expression is invalid.
//
define sealed generic compile-regex
    (pattern :: <string>,
     #key case-sensitive :: <boolean> = #t,
          verbose :: <boolean> = #f,
          multi-line :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f,
          use-cache :: <boolean> = #t)
 => (regex :: <regex>);

define method compile-regex
    (pattern :: <string>,
     #key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f,
          use-cache       :: <boolean> = #t)
 => (regex :: <regex>)
  if (use-cache)
    let cache-key = list(pattern, case-sensitive, verbose, multi-line,
                         dot-matches-all);
    element(*regex-cache*, cache-key, default: #f)
    | begin
        *regex-cache*[cache-key]
          := compile-regex(pattern,
                           case-sensitive: case-sensitive,
                           verbose: verbose,
                           dot-matches-all: dot-matches-all,
                           use-cache: #f);
       end
  else
    parse(pattern,
          make-parse-info(case-sensitive: case-sensitive,
                          verbose: verbose,
                          multi-line: multi-line,
                          dot-matches-all: dot-matches-all))
  end
end method compile-regex;

// Find the position of a regular expression inside a string.  If the
// regex is not found, return #f, otherwise return a variable number
// of marks.  This is a low-level API, returning indices marking the
// start and end of groups.  Use regex-search if you want to get a
// <regex-match> object back.
//
define generic regex-position
    (pattern :: <regex>, text :: <string>,
     #key start :: <integer>,
          end: epos :: <integer>,
          case-sensitive :: <boolean>)
 => (regex-start :: false-or(<integer>), #rest marks);

define method regex-position
    (pattern :: <regex>, text :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = text.size,
          case-sensitive :: <boolean> = #t)
 => (regex-start :: false-or(<integer>), #rest marks :: false-or(<integer>))
  let substring = make(<substring>, string: text, start: start, end: epos);
  let (matched, marks)
    = if (pattern.is-anchored?)
        let searcher = #f;
	anchored-match-root?(pattern, substring, case-sensitive,
			     pattern.regex-group-count, searcher);
      else
	let initial = pattern.initial-substring;
	let searcher = ~initial.empty?
	  & make-substring-positioner(initial, case-sensitive: case-sensitive);
	match-root?(pattern, substring, case-sensitive, pattern.regex-group-count,
		    searcher);
      end if;
  if (matched)  
    apply(values, marks)
  else
    #f
  end
end method regex-position;

// Deprecated.  Use curry(regex-position, regex) or a local method instead.
//
define inline function make-regex-positioner
    (pattern :: <regex>,
     #key case-sensitive :: <boolean> = #t)
 => (regex-positioner :: <function>)
  method (string :: <string>,
          #key start :: <integer> = 0,
               end: epos :: <integer> = string.size)
   => (regex-start :: false-or(<integer>), 
       #rest marks :: false-or(<integer>))
    regex-position(pattern, string,
                   case-sensitive: case-sensitive, 
                   start: start,
                   end: epos);
  end method;
end function make-regex-positioner;

define generic regex-replace
    (big :: <string>, pattern :: <regex>, replacement-text :: <string>,
     #key start :: <integer>,
          end: epos :: <integer>,
          count :: false-or(<integer>),
          case-sensitive :: <boolean>)
 => (new-string :: <string>);

define method regex-replace
    (big :: <string>, pattern :: <regex>, replacement-text :: <string>,
     #key count :: false-or(<integer>),
          start :: <integer> = 0,
          end: epos :: <integer> = big.size,
          case-sensitive :: <boolean> = #t)
 => (new-string :: <string>)
  let positioner
    = make-regex-positioner(pattern, case-sensitive: case-sensitive);
  do-replacement(positioner, replacement-text, big, start,
		 epos, count, #t);
end method regex-replace;

// todo -- Improve error message for <invalid-match-group> errors.
//         Make %s and %= display the regex elided if it's too long.



// Returns a <regex-match> containing info about a successful match, or #f if
// no match was found.
//
// @param pattern -- The regex pattern to search for.
// @param text -- The string in which to search.
// @param anchored -- Whether or not the search should be anchored at the start
//   position.  This is useful because "^..." will only match at the beginning
//   of a string, or after \n if the regex was compiled with multi-line = #t.
// @param start -- Where to begin the search.
// @param end -- Where to stop searching.
// @param case-sensitive -- Whether to be case-sensitive while matching.  Default
//   is #t.  (I don't believe this affects character set (e.g., [a-z]) matching.
//   Check it.)
//
// todo -- Should $ anchor at the provided end position or at the end of the string?
//
define sealed generic regex-search
    (pattern :: <regex>, text :: <string>,
     #key anchored :: <boolean>,
          start :: <integer>,
          end: epos :: <integer>,
          case-sensitive :: <boolean>)
 => (match :: false-or(<regex-match>));

define method regex-search
    (pattern :: <regex>, text :: <string>,
     #key anchored :: <boolean> = #f,
          start    :: <integer> = 0,
          end: epos :: <integer> = text.size,
          case-sensitive :: <boolean> = #t)
 => (match :: false-or(<regex-match>))
  let substring = make(<substring>, string: text, start: start, end: epos);
  let num-groups = pattern.regex-group-count;
  let (matched?, marks)
    = if (pattern.is-anchored?)
        anchored-match-root?(pattern, substring, case-sensitive, num-groups, #f);
      else
        let initial = pattern.initial-substring;
        let searcher = ~initial.empty?
          & make-substring-positioner(initial, case-sensitive: case-sensitive);
        match-root?(pattern, substring, case-sensitive, num-groups, searcher);
      end if;
  if (matched?)
    let regex-match = make(<regex-match>, regular-expression: pattern);
    let group-number-to-name :: <table> = pattern.group-number-to-name;
    for (index from 0 below marks.size by 2)
      let group-number = floor/(index, 2);
      let group-name = element(group-number-to-name, group-number, default: #f);
      let bpos = marks[index];
      let epos = marks[index + 1];
      if (bpos & epos)
        add-group(regex-match,
                  make(<match-group>,
                       text: copy-sequence(text, start: bpos, end: epos),
                       start: bpos,
                       end: epos),
                  group-name);
      else
        // This group wasn't matched.
        add-group(regex-match, #f, group-name);
      end;
    end;
    regex-match
  else
    #f
  end
end method regex-search;

// Like regex-search, but returns a string or #f for each group in the regular
// expression, instead of a <regex-match>.  Note that group 0, the entire match,
// is included as the first value.
define sealed generic regex-search-strings
    (pattern :: <regex>, text :: <string>,
     #key anchored :: <boolean>,
          start :: <integer>,
          end: epos :: <integer>,
          case-sensitive :: <boolean>)
 => (#rest strings);

define method regex-search-strings
    (pattern :: <regex>, text :: <string>,
     #key anchored :: <boolean> = #f,
          start    :: <integer> = 0,
          end: epos :: <integer> = text.size,
          case-sensitive :: <boolean> = #t)
 => (#rest strings)
  let match = regex-search(pattern, text,
			   anchored: anchored,
                           start: start,
                           end: epos,
                           case-sensitive: case-sensitive);
  if (match)
    apply(values, map(method (group)
                        group & group.group-text
                      end,
                      match.groups-by-position))
  else
    #f
  end
end method regex-search-strings;

define sealed class <match-group> (<object>)
  constant slot group-text :: <string>,
    required-init-keyword: text:;
  constant slot group-start :: <integer>,
    required-init-keyword: start:;
  constant slot group-end :: <integer>,
    required-init-keyword: end:;
end class <match-group>;

define sealed class <regex-match> (<object>)
  // Groups by position.  Zero is the entire match.
  constant slot groups-by-position :: <stretchy-vector> = make(<stretchy-vector>);
  // Named groups, if any.  Initial size 0 on the assumption that most regular
  // expressions won't use named groups.
  constant slot groups-by-name :: <string-table> = make(<string-table>, size: 0);
  constant slot regular-expression :: <regex>, required-init-keyword: regular-expression:;
end class <regex-match>;

define method add-group
    (match :: <regex-match>,
     group :: false-or(<match-group>),
     name :: false-or(<string>))
 => (match :: <regex-match>)
  add!(match.groups-by-position, group);
  if (name)
    match.groups-by-name[name] := group;
  end;
  match
end;

define sealed class <invalid-match-group> (<regex-error>)
end class <invalid-match-group>;

// This has methods for group :: <string> and group :: <integer>.
// Group zero is always the entire match.
//
define sealed generic match-group
    (match :: <regex-match>, group :: <object>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>));

define method match-group
    (match :: <regex-match>, group-number :: <integer>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>))
  if (0 <= group-number & group-number < match.groups-by-position.size)
    let group = match.groups-by-position[group-number];
    if (group)
      values(group.group-text, group.group-start, group.group-end)
    else
      values(#f, #f, #f)
    end
  else
    let ng = match.groups-by-position.size;
    signal(make(<invalid-match-group>,
                format-string: "Group number %d is out of bounds for regex %s match.  %s",
                format-arguments: list(group-number,
                                       match.regular-expression.regex-pattern,
                                       if (ng == 1)
                                         "There is only 1 group."
                                       else
                                         format-to-string("There are %d groups.", ng)
                                       end)));
  end;
end method match-group;

define method match-group
    (match :: <regex-match>, group :: <string>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>))
  let group = element(match.groups-by-name, group, default: #f);
  if (group)
    values(group.group-text, group.group-start, group.group-end)
  else
    signal(make(<invalid-match-group>,
                format-string: "There is no group named %=.",
                format-arguments: list(group)));
  end
end method match-group;


//// Utilities

// The split method is exported from the common-dylan module.
//
define method split
    (text :: <string>, separator :: <regex>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = text.size,
          count :: <integer> = epos + 1,
          case-sensitive :: <boolean> = #t,
          remove-if-empty :: <boolean> = #f)
 => (parts :: <sequence>)
  local method find-regex (string :: <string>,
                           bpos :: <integer>,
                           epos :: false-or(<integer>))
          let match = regex-search(separator, string, start: bpos, end: epos);
          if (match)
            let (ignore, match-start, match-end) = match-group(match, 0);
            values(match-start, match-end)
          else
            #f
          end
        end method find-regex;
  split(text, find-regex, start: start, end: epos, count: count,
        remove-if-empty: remove-if-empty)
end method split;

