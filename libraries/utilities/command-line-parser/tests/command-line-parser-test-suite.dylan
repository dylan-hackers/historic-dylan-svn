module: command-line-parser-test-suite
synopsis: Test suite for the command-line-parser  library.

//======================================================================
//
//  Copyright (c) 1998 Eric Kidd
//  All rights reserved.
// 
//  Use and copying of this software and preparation of derivative
//  works based on this software are permitted, including commercial
//  use, provided that the following conditions are observed:
// 
//  1. This copyright notice must be retained in full on any copies
//     and on appropriate parts of any derivative works. (Other names
//     and years may be added, so long as no existing ones are removed.)
// 
//  This software is made available "as is".  Neither the authors nor
//  Carnegie Mellon University make any warranty about the software,
//  its performance, or its conformity to any specification.
// 
//  Bug reports, questions, comments, and suggestions should be sent by
//  E-mail to the Internet address "gd-bugs@gwydiondylan.org".
//
//======================================================================

// Modified by Carl Gay to use the testworks library and to test
// defargparser.  Moved from src/tests to libraries/getopt/tests.
// 2006.11.29

// Now in libraries/utilities/command-line-parser/tests
// Hannes Mehnert 2007.02.23

define suite command-line-parser-test-suite 
  (/* setup-function: foo, cleanup-function: bar */)
  test argument-list-parser-test;
  test defargparser-test;
end suite;


// Create a parser for our standard test argument list, parse the given
// argument list, return the parser.
define function parse (#rest argv)
  let parser = make(<argument-list-parser>);
  // Usage: progname [-qvfB] [-Q arg] [-O [arg]] [-W arg]* [-Dkey[=value]]*
  add-option-parser-by-type(parser,
			    <simple-option-parser>,
			    long-options: #("verbose"),
			    short-options: #("v"),
			    negative-long-options: #("quiet"),
			    negative-short-options: #("q"),
			    default: #t);
  add-option-parser-by-type(parser,
			    <simple-option-parser>,
			    long-options: #("foo"),
			    short-options: #("f"),
			    negative-long-options: #("no-foo"),
			    negative-short-options: #("B"),
			    default: #f);
  add-option-parser-by-type(parser,
			    <parameter-option-parser>,
			    long-options: #("quux"),
			    short-options: #("Q"));
  add-option-parser-by-type(parser,
			    <optional-parameter-option-parser>,
			    long-options: #("optimize"),
			    short-options: #("O"));
  add-option-parser-by-type(parser,
			    <repeated-parameter-option-parser>,
			    long-options: #("warning"),
			    short-options: #("W"));
  add-option-parser-by-type(parser,
			    <keyed-option-parser>,
			    long-options: #("define"),
			    short-options: #("D"));
  values(parser, parse-arguments(parser, argv))
end function parse;
  
define test argument-list-parser-test ()
  let (parser, parse-result) = parse("--frobozz");
  check-equal("parse-arguments returns #f for an unparsable command line",
              parse-result,
              #f);

  let (parser, parse-result) = parse("--quiet");
  check-equal("parse-arguments returns #t for a parsable command line",
              parse-result,
              #t);

  // A correct parse with all arguments specified in long format.
  let (parser, parse-result) = parse("--verbose", "--foo",
                                     "--quux", "quux-value",
                                     "--optimize=optimize-value",
                                     "--warning", "warning-value",
                                     "--define", "key", "=", "value");
  check-equal("verbose is true",
              option-value-by-long-name(parser, "verbose"),
              #t);
  check-equal("foo has correct value",
              option-value-by-long-name(parser, "foo"),
              #t);
  check-equal("quux has correct value",
              option-value-by-long-name(parser, "quux"),
              "quux-value");
  check-equal("optimize has correct value",
              option-value-by-long-name(parser, "optimize"),
              "optimize-value");
  check-equal("warning has correct value",
              option-value-by-long-name(parser, "warning"),
              #("warning-value"));
  let defines = option-value-by-long-name(parser, "define");
  check-equal("key is defined as 'value'", defines["key"], "value");
  check-true("regular arguments are empty", empty?(parser.regular-arguments));

end test argument-list-parser-test;


define argument-parser <defargparser-test-parser> ()
  synopsis print-defargparser-test-synopsis,
    usage: "test [options] file...",
    description: "Stupid test program doing nothing with the args.";
  option verbose?,
    "", "Explanation",
    short: "v",
    long: "verbose";
  option other,
    "", "foo",
    long: "other-option";
  option log-filename,
    "", "Log file pathname",
    kind: <parameter-option-parser>,
    long: "log",
    short: "l";
  regular-arguments file-names;
end argument-parser;


define test defargparser-test ()
  let parser = make(<defargparser-test-parser>);
  parse-arguments(parser, #());
  check-false("Verbose flag is false if not supplied.",
              parser.verbose?);
  check-true("Regular arguments are empty.",
             empty?(parser.file-names));
end test defargparser-test;
