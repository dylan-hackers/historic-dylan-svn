Module: regular-expressions-impl
Author: Carl Gay
Synopsis: A new API for the regular-expressions library


// Rename a few things...
define constant <regex> = <parsed-regexp>;
define constant <invalid-regex> = <illegal-regexp>;
define constant invalid-regex-pattern = regexp-pattern;


// Compile the given string into an optimized regular expression.
//
// @param case-sensitive -- Whether to be case sensivite.
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
// @param dot-matches-newline -- Normally '.' matches any character except for
//   newline.  If this parameter is true '.' matches newline as well.
//
// This function signals <invalid-regex> if the regular expression is invalid.
//
define sealed generic compile-regex
    (string :: <string>,
     #key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f)
 => (regex :: <regex>);

define method compile-regex
    (string :: <string>,
     #key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f)
 => (regex :: <regex>)
  parse(string,
        make-parse-info(case-sensitive: case-sensitive,
                        verbose: verbose,
                        multi-line: multi-line,
                        dot-matches-all: dot-matches-all))
end;


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
    (big :: <string>, pattern :: <object>,
     #key anchored  :: <boolean> = #f,
          start     :: <integer> = 0,
          end: _end :: <integer> = big.size)
 => (match :: false-or(<regex-match>));

define method regex-search
    (big :: <string>, pattern :: <string>,
     #key anchored  :: <boolean> = #f,
          start     :: <integer> = 0,
          end: _end :: <integer> = big.size)
 => (match :: false-or(<regex-match>))
  regex-search(big, compile-regex(pattern),
               anchored: anchored, start: start, end: _end)
end method regex-search;

define method regex-search
    (big :: <string>, pattern :: <regex>,
     #key anchored :: <boolean> = #f,
          start    :: <integer> = 0,
          end: _end :: <integer> = big.size)
 => (match :: false-or(<regex-match>))
  // Copied from regexp-position with some mods to match our interface.
  // Unlike regexp-position there is no caching.  If you don't want to
  // recompile your regex each time, compile it explicitly with compile-regex
  // and save it.
  let substring = make(<substring>, string: big, start: start, end: _end);
  let case-sensitive? = #t;
  let (matched?, marks)
    = if (pattern.is-anchored?)
        anchored-match-root?(pattern, substring, case-sensitive?, last-group + 1, #f);
      else
        let initial = pattern.initial-substring;
        let searcher = ~initial.empty?
          & make-substring-positioner(initial, case-sensitive: case-sensitive?);
        match-root?(pattern, substring, case-sensitive?, last-group + 1, searcher);
      end if;
  if (matched?)
    let regex-match = make(<regex-match>);
    for (index from 0 by 2)
      let bpos = marks[index];
      let epos = marks[index + 1];
      // It would be nice to make <substring> a real sequence, and possibly unify
      // it with the substring implementation in Koala.
      let text = copy-sequence(substring.entire-string,
                               start: substring.start-index + bpos,
                               end: substring.end-index + epos);
      add-group(regex-match, make(<match-group>, text: text, start: bpos, end: epos));
    end;
    regex-match
  else
    #f
  end
end method regex-search;

// This has methods for group :: <string> and group :: <integer>.
// Group zero is always the entire match.
define sealed generic regex-match-group
    (match :: <regex-match>, group :: <object>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>));

// How many groups matched?  There will always be at least one; the entire match.
// (Maybe better to provide a way to iterate over the groups instead, but this
// should be rarely used since you generally know what your groups are.)
//
define sealed generic regex-match-group-count
    (match :: <regex-match>) => (count :: <integer>);


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
  constant slot group-vector :: <stretchy-vector> = make(<stretchy-vector>);
  // Maps group names to positions.
  constant slot group-table  :: <string-table> = make(<string-table>);
end class <regex-match>;

define method add-group
    (match :: <regex-match>, group :: <match-group>,
     #key name :: false-or(<string>))
 => (match :: <regex-match>)
  add!(match.group-vector, group);
  if (name)
    match.group-table[name] := group;
  end;
  match
end;

define sealed class <invalid-match-group> (<regex-error>)
end class <invalid-match-group>;

define method regex-match-group
    (match :: <regex-match>, group :: <integer>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>))
  if (0 <= group < match.group-vector.size)
    match.group-vector[group]
  else
    signal(make(<invalid-match-group>,
                format-string: "Match group index %d out of bounds.  Max group index is %d.",
                format-arguments: list(group, match.group-vector.size - 1)));
  end;
end method regex-match-group;

define method regex-match-group
    (match :: <regex-match>, group :: <string>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>))
  let index = element(match.group-table, group, default: #f);
  if (index)
    regex-match-group(match, index)
  else
    signal(make(<invalid-match-group>,
                format-string: "There is no group named %=.",
                format-arguments: list(group)));
  end;
end method regex-match-group;
