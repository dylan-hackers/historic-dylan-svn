Module:    httpi
Synopsis:  A command-line interface to start Koala as an application.
Author:    Carl Gay
Copyright: Copyright (c) 2001-2008 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

//// Initialization

begin
  add-option-parser-by-type(*argument-list-parser*,
                            <repeated-parameter-option-parser>,
                            description: "ipaddr:port on which to listen for requests",
                            long-options: #("listen"),
                            short-options: #("l"));
  add-option-parser-by-type(*argument-list-parser*,
                            <parameter-option-parser>,
                            description: "Location of the koala configuration file",
                            long-options: #("config"),
                            short-options: #("c"));
  add-option-parser-by-type(*argument-list-parser*,
                            <parameter-option-parser>,
                            description: "Port number on which to listen",
                            long-options: #("port"),
                            short-options: #("p"));
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
end;

// This is defined here rather than in koala-app because wiki needs it too.
//
define function koala-main
    () => ()
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
  else
    block ()
      let port-string = option-value-by-long-name(parser, "port") | "80";
      let listeners = option-value-by-long-name(parser, "listen");
      let server = make(<http-server>,
                        listeners: iff(empty?(listeners),
                                       #["0.0.0.0:80"],
                                       listeners),
                        debug: option-value-by-long-name(parser, "debug"),
                        port: string-to-integer(port-string));
      start-server(server,
                   config-file: option-value-by-long-name(parser, "config"));
    exception (ex :: <error>)
      format(*standard-error*, "Error: %s\n", ex)
    end;
  end if;
end function koala-main;

