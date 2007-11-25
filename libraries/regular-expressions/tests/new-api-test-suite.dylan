Module: new-api-test-suite
Author: Carl Gay


// todo -- why does this fail to match anything if it has a leading space???
define constant $group-regexp = compile-regexp("[0-9]: (.*)|No match");
define constant $group-index-of-what-pcre-matched = 1;

define function run-pcre-checks
    (pathname :: <pathname>) => ()
  with-open-file(stream = pathname, direction: #"input")
    let line = #f;
    let line-number = 0;
    let lines = make(<stretchy-vector>);
    while (line := read-line(stream, on-end-of-stream: #f))
      line-number := line-number + 1;
      line := trim(line, from: #"left");
      if (empty?(line))
        // A section break is always preceded by a group result or "No match".
        // Some multi-line regular expression patterns have empty lines and we
        // don't want to think that's the end of the section.
        if (lines.size > 0
              & regexp-search(lines[lines.size - 1], $group-regexp))
          check-pcre-section(make(<section>,
                                  lines: lines,
                                  start-line-number: line-number - lines.size));
          lines := make(<stretchy-vector>);
        elseif (lines.size > 0) // a section never begins with a blank line
          test-output("Line %d: lines.size = %d, matches = %s, prev = %s\n",
                      line-number,
                      lines.size,
                      regexp-search(lines[lines.size - 1], $group-regexp),
                      lines[lines.size - 1]);
          add!(lines, line);
        end;
      else
        add!(lines, line);
      end;
    end while;
  end;
end function run-pcre-checks;

define class <section> (<object>)
  constant slot section-lines :: <sequence>, required-init-keyword: #"lines";
  constant slot start-line-number :: <integer>, required-init-keyword: #"start-line-number";
  slot %index :: <integer> = 0;
end class <section>;

define method consume-line
    (section :: <section>) => (line :: false-or(<string>))
  let index = section.%index;
  let lines = section.section-lines;
  if (index < lines.size)
    section.%index := index + 1;
    lines[index]
  end
end method consume-line;

define method peek-line
    (section :: <section>) => (line :: false-or(<string>))
  if (section.%index < section.section-lines.size)
    section.section-lines[section.%index]
  end
end method peek-line;

/* Example section:
  /([\da-f:]+)$/i                               // pattern and flags
      0abc                                      // test string
   0: 0abc                                      // group 0
   1: 0abc                                      // group 1
      0zzz                                      // test string
  No match                                      // no match
*/
define function check-pcre-section
    (section :: <section>)
  let regexp = parse-pcre-regexp(section);
  while (peek-line(section))
    let test-string = consume-line(section);
    //test-output("  test string: %s\n", test-string);
    let group-strings = make(<stretchy-vector>);
    block (done-with-this-test-string)
      while (#t)
        let line = peek-line(section);
        let match = line & regexp-search(line, $group-regexp);
        if (match)
          consume-line(section);
          let group-text = regexp-match-group(match, $group-index-of-what-pcre-matched);
          //test-output("   pcre group: %s\n", group-text | "No match");
          if (group-text)
            add!(group-strings, group-text);
          else
            assert(regexp-match-group(match, 0) = "No match",
                   "previous line was 'No match'");
            done-with-this-test-string();
          end;
        else
          done-with-this-test-string();
        end;
      end;
    end;
    if (regexp)
      check-no-errors(format-to-string("search for %s in %s",
                                       test-string, regexp.regexp-pattern),
                      regexp-search(test-string, regexp));
      let match = block ()
                    regexp-search(test-string, regexp)
                  exception (ex :: <error>)
                    #f
                  end;
      if (match)
        compare-to-pcre-results(regexp.regexp-pattern, test-string, match, group-strings);
      end;
    end if;
  end while;
end function check-pcre-section;

define function parse-pcre-regexp
    (section :: <section>)
 => (regexp :: false-or(<regexp>))
  local method find-last (string, char)
          // position(string, char, from-end: #t)
          block (break)
            for (i from string.size - 1 to 0 by -1)
              if (string[i] == char)
                break(i);
              end;
            end;
          end;
        end method find-last;
  // This is imprecise.  If the pattern spans multiple lines this will fail
  // if any but the first and last lines contain the regex delimiter character.
  // Might be nice to add a function to the regexp module to parse aread a perl
  // style regexp from a stream.
  local method read-pattern-and-flags ()
          let pnf = consume-line(section);
          let delim = pnf[0];
          if (find-last(pnf, delim) == 0)
            // it's a multi-line regex
            while(peek-line(section) & ~find-last(peek-line(section), delim))
              pnf := concatenate(pnf, "\n", consume-line(section));
            end;
            if (peek-line(section))
              pnf := concatenate(pnf, "\n", consume-line(section));
            end;
          end;
          let end-delim = find-last(pnf, delim);
          let flags = copy-sequence(pnf, start: end-delim + 1);
          let pattern = copy-sequence(pnf, start: 1, end: end-delim);
          values(pattern, flags)
        end method read-pattern-and-flags;
  let (pattern, flags) = read-pattern-and-flags();
  //test-output("pattern: %s (flags = %s)\n", pattern, flags);
  for (flag in flags)
    check-true(format-to-string("For regex %s, flag %s is recognized",
                                pattern, flag),
               member?(flag, "ixms"));
  end for;
  block ()
    compile-regexp(pattern,
                   case-sensitive: ~ member?('i', flags),
                   verbose: member?('x', flags),
                   multi-line: member?('m', flags),
                   dot-matches-all: member?('s', flags))
  // Unfortunately we can't catch <regexp-error> here because the charset
  // parser is in string-extensions and signals <invalid-character-set-description>
  // which isn't related to <regexp-error> (and isn't even exported).
  exception (ex :: <error>)
    check-true(format-to-string("can compile regex %s", pattern), #f);
    //test-output("  ERROR: %s\n", ex);
    #f
  end block
end function parse-pcre-regexp;
  
/*
 * pcre-groups is a sequence of strings where the nth element represents
 * the nth group in the pcre regexp match.  If pcre-groups is empty then
 * there was no match.
 */
define function compare-to-pcre-results
    (pattern :: <string>,
     test-string :: <string>,
     match :: false-or(<regexp-match>),
     pcre-groups :: <sequence>)
 => ()
  if (match)
    check-equal(format-to-string("Match %s against %s -- same # of groups",
                                 test-string, pattern),
                match.groups-by-position.size,
                pcre-groups.size);
    for (group-number from 0,
         pcre-group in pcre-groups)
      let our-group = regexp-match-group(match, group-number);
      check-equal(format-to-string("Match %s against %s -- group %d is the same",
                                   test-string, pattern, group-number),
                  our-group,
                  pcre-group);
    end;
  else
    check-equal(format-to-string("Pattern %s doesn't match test string %s",
                                 pattern, test-string),
                0,
                pcre-groups.size);
  end if;
end function compare-to-pcre-results;
      
define test pcre-test ()
  let source-directory = environment-variable("OPEN_DYLAN_USER_SOURCES");
  if (source-directory)
    let dir = subdirectory-locator(as(<directory-locator>, source-directory),
                                   "libraries",
                                   "regular-expressions",
                                   "tests");
    run-pcre-checks(make(<file-locator>,
                         directory: dir,
                         name: "pcre-testoutput1.txt"));
  else
    signal(make(<simple-error>,
                format-string: "pcre-test requires the OPEN_DYLAN_USER_SOURCES environment "
                               "variable to be set to the root of your Dylan sources."));
  end;
end test pcre-test;

define suite new-api-test-suite ()
  test pcre-test;
end suite new-api-test-suite;

