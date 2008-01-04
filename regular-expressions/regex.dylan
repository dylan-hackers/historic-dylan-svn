Module: regular-expressions
Author: Carl Gay
Synopsis: A new API for the regular-expressions library

// todo -- Improve error message for <invalid-match-group> errors.
//         Make %s and %= display the regex elided if it's too long.



define constant <invalid-regex> = <illegal-regex>;


// Compile the given string into an optimized regular expression.
//
// @param case-sensitive -- Whether to be case sensitive.
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
// This function signals <invalid-regex> if the regular expression is invalid.
//
define sealed generic compile-regex
    (pattern :: <string>,
     #key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f)
 => (regex :: <regex>);

define method compile-regex
    (pattern :: <string>,
     #key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f)
 => (regex :: <regex>)
  parse(pattern,
        make-parse-info(case-sensitive: case-sensitive,
                        verbose: verbose,
                        multi-line: multi-line,
                        dot-matches-all: dot-matches-all))
end method compile-regex;


// Returns a <regex-match> containing info about a successful match, or #f if
// no match was found.
//
// @param big -- The string in which to search.
// @param pattern -- The pattern to search for.  If not a <regex>, it will be
//   compiled first with compile-regex (implying that <invalid-regex> may be
//   signalled), using the defaults for the keyword arguments.  If you wish
//   to override them, call compile-regex directly.
// @param anchored -- Whether or not the search should be anchored at the start
//   position.  This is useful because "^..." will only match at the beginning
//   of a string, or after \n if the regex was compiled with multi-line = #t.
// @param start -- Where to begin the search.
// @param end -- Where to stop searching.
//
// todo -- Should $ anchor at the provided end position or at the end of the string?
//
define sealed generic regex-search
    (pattern :: <object>, string :: <string>,
     #key anchored  :: <boolean> = #f,
          start     :: <integer> = 0,
          end: _end :: <integer> = big.size)
 => (match :: false-or(<regex-match>));

define method regex-search
    (pattern :: <string>, string :: <string>,
     #key anchored  :: <boolean> = #f,
          start     :: <integer> = 0,
          end: _end :: <integer> = string.size)
 => (match :: false-or(<regex-match>))
  regex-search(compile-regex(pattern),
               string,
               anchored: anchored, start: start, end: _end)
end method regex-search;

define method regex-search
    (pattern :: <regex>, string :: <string>,
     #key anchored :: <boolean> = #f,
          start    :: <integer> = 0,
          end: _end :: <integer> = string.size)
 => (match :: false-or(<regex-match>))
  // Unlike regex-position there is no caching.  If you don't want to
  // recompile your regex each time, compile it explicitly with compile-regex
  // and save it.
  let substring = make(<substring>, string: string, start: start, end: _end);
  let case-sensitive? = #t;
  let num-groups = pattern.regex-group-count;
  let (matched?, marks)
    = if (pattern.is-anchored?)
        anchored-match-root?(pattern, substring, case-sensitive?, num-groups, #f);
      else
        let initial = pattern.initial-substring;
        let searcher = ~initial.empty?
          & make-substring-positioner(initial, case-sensitive: case-sensitive?);
        match-root?(pattern, substring, case-sensitive?, num-groups, searcher);
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
        // It would be nice to make <substring> a real sequence, and possibly unify
        // it with the substring implementation in Koala.
        let text = copy-sequence(substring.entire-string,
                                 start: substring.start-index + bpos,
                                 end: substring.start-index + epos);
        add-group(regex-match,
                  make(<match-group>, text: text, start: bpos, end: epos),
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

define sealed generic regex-search-strings
    (pattern :: <object>, string :: <string>,
     #key anchored  :: <boolean> = #f,
     start     :: <integer> = 0,
     end: _end :: <integer> = big.size)
 => (#rest strings :: false-or(<string>));

define method regex-search-strings
    (pattern :: <string>, string :: <string>,
     #key anchored  :: <boolean> = #f,
     start     :: <integer> = 0,
     end: _end :: <integer> = string.size)
 => (#rest strings :: false-or(<string>))
  regex-search-strings(compile-regex(pattern),
                       string,
		       anchored: anchored, start: start, end: _end)
end method regex-search-strings;

define method regex-search-strings
    (pattern :: <regex>, string :: <string>,
     #key anchored :: <boolean> = #f,
     start    :: <integer> = 0,
     end: _end :: <integer> = string.size,)
 => (#rest strings :: false-or(<string>))
  let match = regex-search(pattern,
			   string, 
			   anchored: anchored, start: start, end: _end);
  if (match)
    apply(values, map(method (group) group & group.group-text end,
                      match.groups-by-position))
  else
    values(match)
  end if;
end method regex-search-strings;

// Get the groups for the match.  There will always be at least one; the entire match.
//
define sealed generic match-groups
    (match :: <regex-match>) => (groups :: <sequence>);

define method match-groups
    (match :: <regex-match>) => (groups :: <sequence>)
  map-as(<simple-object-vector>, identity, match.groups-by-position)
end;

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
