Module:    httpi
Synopsis:  Library initialization code
Author:    Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

//// Testing

define constant $debugging-koala :: <boolean> = #f;

define function test-koala
    () => ()
  // Nothing yet...
end;


//// Initialization

define function init-koala ()
  when ($debugging-koala)
    test-koala();
  end;

  add-option-parser-by-type(*argument-list-parser*,
                            <parameter-option-parser>,
                            description: "Location of the koala configuration file",
                            long-options: #("config"),
                            short-options: #("c"));
  add-option-parser-by-type(*argument-list-parser*,
                            <simple-option-parser>,
                            description: "Display this help message",
                            long-options: #("help"),
                            short-options: #("h"));
  add-option-parser-by-type(*argument-list-parser*,
                            <simple-option-parser>,
                            description: "Enable debugging.  Causes Koala to not handle "
                                         "most errors during request handling.",
                            long-options: #("debug"));

  //init-server();
end;

begin
  init-koala();
end;

// This is defined here rather than in koala-app because wiki needs it too.
define function koala-main ()
  let parser = *argument-list-parser*;
  parse-arguments(parser, application-arguments());
  if (option-value-by-long-name(parser, "help")
        | ~empty?(parser.regular-arguments))
    let desc = "The Koala web server, a multi-threaded web server with\n"
      "Dylan Server Pages and XML RPC, written in Dylan.";
    print-synopsis(parser,
                   *standard-output*,
                   usage: "koala [options]",
                   description: desc);
    exit-application(0);
  else
    if (option-value-by-long-name(parser, "debug"))
      *debugging-server* := #t;
    end;
    start-server(config-file: option-value-by-long-name(parser, "config"));
  end;
end function koala-main;
