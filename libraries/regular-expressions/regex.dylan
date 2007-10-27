Module: regular-expressions-impl
Author: Carl Gay
Synopsis: A new API for the regular-expressions library


// Rename a few things...
define constant <invalid-regexp> = <illegal-regexp>;
define constant invalid-regexp-pattern = regexp-pattern;


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
// This function signals <invalid-regexp> if the regular expression is invalid.
//
define sealed generic compile-regexp
    (string :: <string>,
     #key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f)
 => (regexp :: <regexp>);

define method compile-regexp
    (string :: <string>,
     #key case-sensitive  :: <boolean> = #t,
          verbose         :: <boolean> = #f,
          multi-line      :: <boolean> = #f,
          dot-matches-all :: <boolean> = #f)
 => (regexp :: <regexp>)
  parse(string,
        make-parse-info(case-sensitive: case-sensitive,
                        verbose: verbose,
                        multi-line: multi-line,
                        dot-matches-all: dot-matches-all))
end method compile-regexp;


// Returns a <regexp-match> containing info about a successful match, or #f if
// no match was found.
//
// @param big -- The string in which to search.
// @param pattern -- The pattern to search for.  If not a <regexp>, it will be
//   compiled first with compile-regexp (implying that <invalid-regexp> may be
//   signalled), using the defaults for the keyword arguments.  If you wish
//   to override them, call compile-regexp directly.
// @param anchored -- Whether or not the search should be anchored at the start
//   position.  This is useful because "^..." will only match at the beginning
//   of a string, or after \n if the regexp was compiled with multi-line = #t.
// @param start -- Where to begin the search.
// @param end -- Where to stop searching.
//
// todo -- Should $ anchor at the provided end position or at the end of the string?
//
define sealed generic regexp-search
    (big :: <string>, pattern :: <object>,
     #key anchored  :: <boolean> = #f,
          start     :: <integer> = 0,
          end: _end :: <integer> = big.size)
 => (match :: false-or(<regexp-match>));

define method regexp-search
    (big :: <string>, pattern :: <string>,
     #key anchored  :: <boolean> = #f,
          start     :: <integer> = 0,
          end: _end :: <integer> = big.size)
 => (match :: false-or(<regexp-match>))
  regexp-search(big, compile-regexp(pattern),
               anchored: anchored, start: start, end: _end)
end method regexp-search;

define method regexp-search
    (big :: <string>, pattern :: <regexp>,
     #key anchored :: <boolean> = #f,
          start    :: <integer> = 0,
          end: _end :: <integer> = big.size)
 => (match :: false-or(<regexp-match>))
  // Copied from regexp-position with some mods to match our interface.
  // Unlike regexp-position there is no caching.  If you don't want to
  // recompile your regexp each time, compile it explicitly with compile-regexp
  // and save it.
  let substring = make(<substring>, string: big, start: start, end: _end);
  let case-sensitive? = #t;
  let num-groups = pattern.regexp-group-count;
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
    let regexp-match = make(<regexp-match>);
    for (index from 0 below marks.size by 2)
      let bpos = marks[index];
      let epos = marks[index + 1];
      // It would be nice to make <substring> a real sequence, and possibly unify
      // it with the substring implementation in Koala.
      let text = copy-sequence(substring.entire-string,
                               start: substring.start-index + bpos,
                               end: substring.start-index + epos);
      add-group(regexp-match, make(<match-group>, text: text, start: bpos, end: epos));
    end;
    regexp-match
  else
    #f
  end
end method regexp-search;

// This has methods for group :: <string> and group :: <integer>.
// Group zero is always the entire match.
define sealed generic regexp-match-group
    (match :: <regexp-match>, group :: <object>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>));

// Get the groups for the match.  There will always be at least one; the entire match.
//
define sealed generic regexp-match-groups
    (match :: <regexp-match>) => (groups :: <sequence>);

define method regexp-match-groups
    (match :: <regexp-match>) => (groups :: <sequence>)
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

define sealed class <regexp-match> (<object>)
  // Groups by position.  Zero is the entire match.
  constant slot groups-by-position :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot groups-by-name :: <string-table> = make(<string-table>);
end class <regexp-match>;

define method add-group
    (match :: <regexp-match>, group :: <match-group>,
     #key name :: false-or(<string>))
 => (match :: <regexp-match>)
  add!(match.groups-by-position, group);
  if (name)
    match.groups-by-name[name] := group;
  end;
  match
end;

define sealed class <invalid-match-group> (<regexp-error>)
end class <invalid-match-group>;

define method regexp-match-group
    (match :: <regexp-match>, group-number :: <integer>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>))
  if (0 <= group-number & group-number < match.groups-by-position.size)
    let group = match.groups-by-position[group-number];
    values(group.group-text, group.group-start, group.group-end)
  else
    signal(make(<invalid-match-group>,
                format-string: "Match group index %d out of bounds.  Max group index is %d.",
                format-arguments: list(group-number, match.groups-by-position.size - 1)));
  end;
end method regexp-match-group;

define method regexp-match-group
    (match :: <regexp-match>, group :: <string>)
 => (text :: false-or(<string>),
     start-index :: false-or(<integer>),
     end-index :: false-or(<integer>))
  let index :: <integer> = element(match.groups-by-name, group, default: #f);
  if (index)
    regexp-match-group(match, index)
  else
    signal(make(<invalid-match-group>,
                format-string: "There is no group named %=.",
                format-arguments: list(group)));
  end;
end method regexp-match-group;
