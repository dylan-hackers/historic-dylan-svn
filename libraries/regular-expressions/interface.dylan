module:   regular-expressions
author:   Nick Kramer (nkramer@cs.cmu.edu)
synopsis: This provides a useable interface for users. Functions 
	  defined outside this file are really too strange and quirky 
          to be of use to people.
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

// There are quite a few make-fooer functions hanging around.  Now
// that regex-position does caching, these are basically useless, but
// we've kept them around for backwards compatibility.  Unfortunately,
// internally most of the functions are implemented in terms of
// make-regex-positioner.  To minimize the amount of rewriting, I've
// liberally applied seals and inline declarations so that
// make-regex-positioner won't clobber all type information.  The
// downside, of course, is that everything's sealed, but hey, no one
// ever subclassed regex-position anyway.


// Caching
//
// Parsing a regex is not cheap, so we cache the parsed regexs and
// only parse a string if we haven't seen it before.  Because in
// practice almost all regex strings are string literals, we're free
// to choose == or = depending on whatever's fastest.  However,
// because a string is parsed differently depending on whether the
// search is case sensitive or not, we also have to keep track of that
// information as well.  (The case dependent parse boils down to the
// parse creating a <character-set>, which must be either case
// sensitive or case insensitive)
//

// This caching scheme fails if we later introduce the ability to change
// attributes such as case-sensitivity mid-parse, the way (I believe) perl
// does?  --cgay

// ### Currently, only regex-position uses this cache, because the
// other functions are still using make-regex-positioner.  With
// caching, that make-regex-whatever stuff should probably go.

// <cache-key> -- internal
//
// What we use for keys in the *regex-cache*.
//
define class <cache-key> (<object>)
  constant slot regex-string :: <string>, 
    required-init-keyword: #"regex-string";
  constant slot character-set-type :: <class>, 
    required-init-keyword: #"character-set-type";
end class <cache-key>;

// <cache-element> -- internal
//
// What we use for elements in a *regex-cache*
//
define class <cache-element> (<object>)
  constant slot parse-tree :: <parsed-regex>,
    required-init-keyword: #"parse-tree";
  constant slot last-group :: <integer>,
    required-init-keyword: #"last-group";
end class <cache-element>;

// <regex-cache> -- internal
//
// Maps <cache-key> to <cache-element>.  ### Ideally, we'd be using
// weak pointers to these strings.  In practice, however, most of the
// regex strings are literals, so this isn't usually a drawback.
//
// This used to compare strings with == rather than =, but this leaks
// lots of memory
// 
define class <regex-cache> (<table>) end;

// table-protocol{<regex-cache>} -- method on imported G.F.
//
define method table-protocol (table :: <regex-cache>) 
 => (equal? :: <function>, hash :: <function>);
  values(method (key1 :: <cache-key>, key2 :: <cache-key>) // equal?
	  => res :: <boolean>;
           key1.regex-string = key2.regex-string
             & key1.character-set-type == key2.character-set-type;
	 end method,
	 method (key :: <cache-key>, initial-state) => (id :: <integer>, state); // hash()
	   let (string-id, string-state) = object-hash(key.regex-string, initial-state);
	   let (set-type-id, set-type-state) 
	     = object-hash(key.character-set-type, string-state);
	   values(merge-hash-ids(string-id, set-type-id, ordered: #t), set-type-state);
	 end method);
end method table-protocol;

// *regex-cache* -- internal
//
// The only instance of <regex-cache>.  ### Not threadsafe.
// 
// Technically not thread safe, but does it matter?  Worst case seems to
// be a duplicated regex parse.  --cgay
//
define constant *regex-cache* = make(<regex-cache>);

// parse-or-use-cached -- internal
//
// Tries to use the cached version of the regex, and if not possible,
// parses it and adds it to the cache.
//
define inline function parse-or-use-cached 
    (regex :: <string>, parse-info :: <parse-info>)
 => (parsed-regex :: <parsed-regex>, last-group :: <integer>);
  let key = make(<cache-key>, regex-string: regex, 
		 character-set-type: parse-info.set-type); 
  let cached-value = element(*regex-cache*, key, default: #f);
  if (cached-value)
    values(cached-value.parse-tree, cached-value.last-group);
  else
    let (parsed-regex, last-group) = parse(regex, parse-info);
    *regex-cache*[key] := make(<cache-element>,
                                parse-tree: parsed-regex,
				last-group: last-group);
    values(parsed-regex, last-group);
  end if;
end function parse-or-use-cached;


// regex positioner stuff

// Find the position of a regular expression inside a string.  If the
// regex is not found, return #f, otherwise return a variable number
// of marks.
//
define function regex-position
    (big :: <string>, regex :: <string>, #key start: big-start = 0,
     end: big-end = #f, case-sensitive = #f)
 => (regex-start :: false-or(<integer>), #rest marks :: false-or(<integer>));
  let substring = make(<substring>, string: big, start: big-start,
		       end: big-end | big.size);
  let (parsed-regex, last-group) 
    = parse-or-use-cached(regex, make-parse-info(case-sensitive: case-sensitive));

  let (matched, marks)
    = if (parsed-regex.is-anchored?)
	anchored-match-root?(parsed-regex, substring, case-sensitive,
			     last-group + 1, #f);
      else
	let initial = parsed-regex.initial-substring;
	let searcher = ~initial.empty?
	  & make-substring-positioner(initial, case-sensitive: case-sensitive);
	match-root?(parsed-regex, substring, case-sensitive, last-group + 1,
		    searcher);
      end if;
  if (matched)  
    apply(values, marks);
  else
    #f  
  end if;
end function regex-position;

// Once upon a time, this was how you interfaced to the NFA stuff
// (maximum-compile: #t).  That's gone.  Now it's just here for
// backwards compatibility.  All keywords except case-sensitive are
// now ignored.
//
define inline function make-regex-positioner
    (regex :: <string>, 
     #key byte-characters-only = #f, need-marks = #t, maximum-compile = #f,
     case-sensitive = #f)
 => regex-positioner :: <function>;
  method (big :: <string>, #key start: big-start = 0,
	  end: big-end = #f)
   => (regex-start :: false-or(<integer>), 
       #rest marks :: false-or(<integer>));
    regex-position(big, regex, case-sensitive: case-sensitive, 
		    start: big-start, end: big-end);
  end method;
end function make-regex-positioner;

// returns #f if no match, the matching string on match, and another string or #f
// for each group in the regex.
define method regex-match
    (big :: <string>, regex :: <string>) => (#rest results);
  let (#rest marks) = regex-position(big, regex);
  let result = make(<stretchy-vector>);

  if(marks[0])
    for(i from 0 below marks.size by 2)
      if(marks[i] & marks[i + 1])
        result := add!(result, copy-sequence(big, 
                                             start: marks[i], 
                                             end: marks[i + 1]))
      else
        result := add!(result, #f)
      end
    end
  end;
  apply(values, result)
end;

// #if (have-free-time)
/*
// regex-matches -- exported
//
// A more convenient form of regex-position.  Usually you want
// substrings that were matched by a group rather than the marks for
// the group.  How you use this is you give the group numbers you
// want, and it'll give you the strings.  (#f if that group wasn't
// matched)
//
define function regex-matches
    (big :: <string>, regex :: <string>,
     #key start: start-index :: <integer> = 0,
          end: end-index :: false-or(<integer>),
          case-sensitive :: <boolean> = #f,
          groups :: false-or(<sequence>))
 => (#rest group-strings :: false-or(<string>));
  if (~groups)
    error("Mandatory keyword groups: not used in call to regex-matches");
  end if;
  let (#rest marks)
    = regex-position(big, regex, start: start-index, end: end-index, 
		      case-sensitive: case-sensitive);
  let return-val = make(<vector>, size: groups.size, fill: #f);
  for (index from 0 below return-val.size)
    let group-start = groups[index] * 2;
    let group-end = group-start + 1;
    if (element(marks, group-start, default: #f))
      return-val[index] := copy-sequence(big, start: 

  let sz = floor/(marks.size, 2);
  let return = make(<vector>, size: sz, fill: #f);
  for (index from 0 below sz)
    let pos = index * 2;
    if (element(marks, pos, default: #f))
      return[index] := copy-sequence(big, start: marks[pos],
				     end: marks[pos + 1]);
    end if;
  end for;
  if (matches)
    let return = make(<vector>, size: matches.size * 2);
    for (raw-pos in matches, index from 0)
      let src-pos = raw-pos * 2;
      let dest-pos = index * 2;
      return[dest-pos] := element(marks, src-pos, default: #f);
      return[dest-pos + 1] := element(marks, src-pos + 1, default: #f);
    end for;
    apply(values, return);
  else
    
    apply(values, marks);
  end if;

// #endif
*/


// Functions based on regex-position

define function regex-replace
    (input :: <string>, regex :: <string>, new-substring :: <string>,
     #key count = #f, case-sensitive = #f, start = 0, end: input-end = #f)
 => changed-string :: <string>;
  let positioner
    = make-regex-positioner(regex, case-sensitive: case-sensitive);
  do-replacement(positioner, new-substring, input, start, 
		 input-end, count, #t);
end function regex-replace;

define inline function make-regex-replacer 
    (regex :: <string>, #key replace-with, case-sensitive = #f)
 => replacer :: <function>;
  let positioner
    = make-regex-positioner(regex, case-sensitive: case-sensitive);
  if (replace-with)
    method (input :: <string>, #key count: count, 
	    start = 0, end: input-end = #f)
     => string :: <string>;
      do-replacement(positioner, replace-with, input, start, 
		     input-end, count, #t);
    end method;
  else
    method (input :: <string>, new-substring :: <string>, 
	    #key count = #f, start = 0, end: input-end = #f)
     => string :: <string>;
      do-replacement(positioner, new-substring, input, 
		     start, input-end, count, #t);
    end method;
  end if;
end function make-regex-replacer;

// Like Perl's split function
//
define function split
    (pattern :: <string>, input :: <string>, 
     #key count = #f, remove-empty-items = #t, start = 0, end: input-end = #f)
 => (#rest whole-bunch-of-strings :: <string>);
  let positioner = make-regex-positioner(pattern);
  split-string(positioner, input, start, input-end | size(input),
	       count, remove-empty-items);
end function split;

define inline function make-splitter
    (pattern :: <string>) => splitter :: <function>;
  let positioner = make-regex-positioner(pattern);
  method (string :: <string>, #key count = #f,
	  remove-empty-items = #t, start = 0, end: input-end = #f)
   => (#rest whole-bunch-of-strings :: <string>);
    split-string(positioner, string, start, input-end | size(string), 
		 count, remove-empty-items);
  end method;
end function make-splitter;

// Used by split.  Not exported.  (Yes it is.  --cgay)
//
define function split-string
    (positioner :: <function>, input :: <string>, start :: <integer>, 
     input-end :: <integer>, count :: false-or(<integer>), 
     remove-empty-items :: <object>)
 => (#rest whole-bunch-of-strings :: <string>);
  let strings = make(<deque>);
  block (done)
    let end-of-last-match = 0;
    let start-of-where-to-look = start;
    let string-number = 1;    // Since count: starts at 1, so 
                              // should string-number
    while (#t)
      let (substring-start, substring-end)
	= positioner(input, start: start-of-where-to-look, end: input-end);
      if (~substring-start | (count & (count <= string-number)))
	push-last(strings, copy-sequence(input, start: end-of-last-match));
	done(); 
      elseif ((substring-start = start-of-where-to-look)
		&  remove-empty-items)
	      // delimited item is empty
	end-of-last-match := substring-end;
	start-of-where-to-look := end-of-last-match;
      else
	let new-string = copy-sequence(input, start: end-of-last-match, 
				       end: substring-start);
	if (~new-string.empty? | ~remove-empty-items)
	  push-last(strings, new-string);
	  string-number := string-number + 1;
	  end-of-last-match := substring-end;
	  start-of-where-to-look := end-of-last-match;
	end if;
      end if;
    end while;
  end block;
  if (remove-empty-items)
    apply(values, remove!(strings, #f, test: method (a, b) a.empty? end));
  else
    apply(values, strings);
  end if;
end function split-string;

// join--like Perl's join
//
// This is not really any more efficient than concatenate-as, but it's
// more convenient.
//
define function join (delimiter :: <byte-string>, #rest strings)
 => big-string :: <byte-string>;
  let length = max(0, (strings.size - 1 ) * delimiter.size);
  for (string in strings)
    length := length + string.size;
  end for;
  let big-string = make(<byte-string>, size: length);
  let big-index = 0;
  for (i from 0 to strings.size - 2)  // Don't iterate over the last string
    let string = strings[i];
    let new-index = big-index + string.size;
    big-string := replace-subsequence!(big-string, string, 
				       start: big-index, end: new-index);
    big-index := new-index;
    let new-index = big-index + delimiter.size;
    big-string := replace-subsequence!(big-string, delimiter, 
				       start: big-index, end: new-index);
    big-index := new-index;
  end for;
  if (strings.size > 0)
    big-string 
      := replace-subsequence!(big-string, strings.last, 
			      start: big-index, end: big-string.size);
  end if;
  big-string;
end function join;


