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
                            description: format-to-string("ipaddr:port on which to "
                                                          "listen for requests.  "
                                                          "[default: 0.0.0.0:%d]",
                                                          $default-http-port),
                            long-options: #("listen"),
                            short-options: #("l"));
  add-option-parser-by-type(*argument-list-parser*,
                            <parameter-option-parser>,
                            description: "Location of the koala configuration file.  "
                                         "[default: None]",
                            long-options: #("config"),
                            short-options: #("c"));
  add-option-parser-by-type(*argument-list-parser*,
                            <simple-option-parser>,
                            description: "Display this help message",
                            long-options: #("help"),
                            short-options: #("h"));
  add-option-parser-by-type(*argument-list-parser*,
                            <simple-option-parser>,
                            description: "Enable debug mode.  Causes Koala to not handle "
                                         "most errors during request handling.",
                            long-options: #("debug"));
end;

// This is defined here rather than in koala-app because wiki needs it too.
//
define function koala-main
    (#key server :: false-or(<http-server>),
          description :: false-or(<string>))
 => ()
  let parser = *argument-list-parser*;
  parse-arguments(parser, application-arguments());
  if (option-value-by-long-name(parser, "help")
        | ~empty?(parser.regular-arguments))
    let desc = description
                 | "The Koala web server, a multi-threaded web server with\n"
                   "Dylan Server Pages and XML RPC, written in Dylan.";
    print-synopsis(parser,
                   *standard-output*,
                   usage: "koala [options]",
                   description: desc);
  else
    block ()
      let _server = server | make(<http-server>);

      // Configure first so that command-line argument override config settings.
      let config-file = option-value-by-long-name(parser, "config");
      if (config-file)
        configure-server(server, config-file);
      end;

      // Setup listeners.
      let listeners = option-value-by-long-name(parser, "listen");
      if (empty?(listeners) & ~config-file)
        listeners := vector(format-to-string("0.0.0.0:%d", $default-http-port));
      end;
      for (listener in listeners)
        add!(_server.server-listeners, make-listener(listener));
      end;

      _server.debugging-enabled? := option-value-by-long-name(parser, "debug");
      if (_server.debugging-enabled?)
        log-warning("Debug mode enabled via command line.");
      end;
      start-server(_server);

    exception (ex :: <error>)
      format(*standard-error*, "Error: %s\n", ex)
    end;
  end if;
end function koala-main;

