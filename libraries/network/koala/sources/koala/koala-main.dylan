Module:    httpi
Synopsis:  Initialization and startup
Author:    Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//// Initialization

define argument-parser <koala-command-line-parser> ()
    option config-file = #f,
      "", "Location of the Koala configuration file",
      short: "c",
      long: "config",
      kind: <parameter-option-parser>;
    option display-help?,
      "", "Display this help message",
      short: "h",
      long: "help";
    option debug-koala?,
      "", "Enabled debugging.  Causes Koala to not handle most errors during "
          "request handling.",
      long: "debug";
    option listen-port,
      "", "Port on which to listen for HTTP requests.",
      short: "p",
      long: "port",
      kind: <parameter-option-parser>;
end argument-parser <koala-command-line-parser>;

// Command-line arguments parser.  The expectation is that libraries that use
// and extend koala (e.g., wiki) may want to add their own <option-parser>s to
// this before calling koala-main().
//
define variable *command-line-parser* :: <koala-command-line-parser>
  = make(<koala-command-line-parser>);

// Parse the command line and start the server.
//
define function koala-main (#key description, wait? = #t)
  let parser = *command-line-parser*;
  parse-arguments(parser, application-arguments());
  if (parser.display-help?
      | ~empty?(parser.regular-arguments))
    let desc = description | "The Koala web server, a multi-threaded web server with\n"
                             "Dylan Server Pages and XML RPC, written in Dylan.";
    print-synopsis(parser,
                   stream: *standard-output*,
                   usage: format-to-string("%s [options]", application-name()),
                   description: desc);
    exit-application(0);
  else
    let config = make(<http-server-configuration>,
                      config-file: config-file(parser),
                      port: listen-port(parser),
                      debug?: debug-koala?(parser));
    let server = make(<http-server>, configuration: config);
    start-server(server, config, wait?: #t);
  end;
end function koala-main;

