module: regular-expressions-test-suite

//// Tests based on PCRE test output files

// todo -- why does this fail to match anything if it has a leading space???
define constant $group-regex = compile-regex("[0-9]: (.*)|No match");
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
              & regex-search($group-regex, lines[lines.size - 1]))
          check-pcre-section(make(<section>,
                                  lines: lines,
                                  start-line-number: line-number - lines.size));
          lines := make(<stretchy-vector>);
        elseif (lines.size < 3)
          // A section must have a regex, a test string and one group result to
          // make any sense.  Note we don't add the line to the section here.
          test-output("Line %d: empty line in first 3 lines of a section.\n",
                      line-number);
        else
          add!(lines, line);
        end;
      else
        add!(lines, line);
      end;
    end while;
  end;
end function run-pcre-checks;

define class <section> (<object>)
  constant slot section-lines :: <sequence>,
    required-init-keyword: #"lines";
  // not used yet...
  // constant slot start-line-number :: <integer>,
  //  required-init-keyword: #"start-line-number";
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

/* Was going to be used for better check names.
define method line-number
    (section :: <section>) => (line-number :: <integer>)
  section.start-line-number + section.%index
end method line-number;
*/

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
  let regex = parse-pcre-regex(section);
  // If the section has fewer than 3 lines (a regex, a test string and at least
  // one group result) then all we do is try to compile it (above).
  if (section.section-lines.size >= 3)
    while (peek-line(section))
      let test-string = consume-line(section);
      //test-output("  test string: %s\n", test-string);
      let group-strings = make(<stretchy-vector>);
      block (done-with-this-test-string)
        while (#t)
          let line = peek-line(section);
          let match = line & regex-search($group-regex, line);
          if (match)
            consume-line(section);
            let group-text = match-group(match,
                                         $group-index-of-what-pcre-matched);
            //test-output("   pcre group: %s\n", group-text | "No match");
            if (group-text)
              add!(group-strings, group-text);
            else
              assert(match-group(match, 0) = "No match",
                     "previous line was 'No match'");
              done-with-this-test-string();
            end;
          else
            done-with-this-test-string();
          end;
        end;
      end;
      if (regex)
        check-no-errors(sprintf("search for %s in %s",
                                test-string, regex.regex-pattern),
                        regex-search(regex, test-string));
        let match = block ()
                      regex-search(regex, test-string)
                    exception (ex :: <error>)
                      #f
                    end;
        if (match)
          compare-to-pcre-results(regex.regex-pattern, test-string, match,
                                  group-strings);
        end;
      end if;
    end while;
  end if;
end function check-pcre-section;

define function parse-pcre-regex
    (section :: <section>)
 => (regex :: false-or(<regex>))
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
  // Might be nice to add a function to the regular-expressions module to read
  // a perl regexp from a stream.
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
  //test-output("pattern: '%s' (flags = '%s')\n", pattern, flags);
  for (flag in flags)
    check-true(sprintf("For regex '%s', flag '%s' is recognized", pattern, flag),
               member?(flag, "ixms"));
  end for;
  block ()
    compile-regex(pattern,
                  case-sensitive: ~ member?('i', flags),
                  verbose: member?('x', flags),
                  multi-line: member?('m', flags),
                  dot-matches-all: member?('s', flags))
  // Unfortunately we can't catch <regex-error> here because the charset
  // parser is in string-extensions and signals
  // <invalid-character-set-description> which isn't related to <regex-error>
  // (and isn't even exported).
  exception (ex :: <error>)
    check-true(sprintf("can compile regex '%s'", pattern), #f);
    //test-output("  ERROR: %s\n", ex);
    #f
  end block
end function parse-pcre-regex;
  
/*
 * pcre-groups is a sequence of strings where the nth element represents
 * the nth group in the pcre regex match.  If pcre-groups is empty then
 * there was no match.
 */
define function compare-to-pcre-results
    (pattern :: <string>,
     test-string :: <string>,
     match :: false-or(<regex-match>),
     pcre-groups :: <sequence>)
 => ()
  if (match)
    check-equal(sprintf("Match '%s' against regex '%s' -- same # of groups",
                        test-string, pattern),
                size(match-groups(match)),
                pcre-groups.size);
    for (group-number from 0,
         pcre-group in pcre-groups)
      // Adding block/exception here causes an infinite loop.
      // Could it be related to using the Visual Studio 8 linker?
      // The if also causes an infinite loop.  Hmmm.
      let our-group = /* if (group-number < size(match-groups(match))) */
                        match-group(match, group-number)
                      /* end */;
      check-equal(sprintf("Match '%s' against regex '%s' -- group %d is the same",
                          test-string, pattern, group-number),
                  our-group,
                  pcre-group);
    end;
  else
    check-equal(sprintf("Regex '%s' doesn't match test string '%s'",
                        pattern, test-string),
                0,
                pcre-groups.size);
  end if;
end function compare-to-pcre-results;

define function make-pcre-locator
    (filename :: <string>) => (locator :: <file-locator>)
  let source-directory = environment-variable("OPEN_DYLAN_USER_SOURCES");
  if (source-directory)
    let dir = subdirectory-locator(as(<directory-locator>, source-directory),
                                   "libraries",
                                   "regular-expressions",
                                   "tests");
    make(<file-locator>, directory: dir, name: filename)
  else
    signal(make(<simple-error>,
                format-string: "pcre-test requires the OPEN_DYLAN_USER_SOURCES environment "
                               "variable to be set to the root of your Dylan sources."));
  end
end function make-pcre-locator;


