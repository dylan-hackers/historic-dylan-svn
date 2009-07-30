Module: httpi
Synopsis:  CGI script handling
Author:    Carl Gay
Copyright: Copyright (c) 2009 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define method cgi-script-responder
    (script :: <locator>)
  let command = as(<string>, script);
  log-debug("cgi-script-responder(%=)", command);
  let request :: <request> = current-request();
  let env :: <string-table> = make-cgi-environment(script);

  for (value keyed-by key in env)
    log-debug("  %s: %s", key, value);
  end;
  
  // Note: when passing a sequence of strings to run-application one
  //       must use as(limited(<vector>, of: <string>), list(command))
  let (exit-code, signal, child, stdout, stderr)
    = run-application(command,
                      asynchronous?: #t,
                      under-shell?: #f,
                      inherit-console?: #f,
                      environment: env,
                      // TODO: see if RFC sez anything about this
                      //working-directory: foo,
                      input: null:,
                      output: stream:,
                      error: stream:);
                      
                      // Windows options, ignored on posix systems
                      //activate?: #f,
                      //minimize?: #t,
                      //hide?: #t);
  log-debug("CGI launched: %s, exit-code: %s, signal: %s, child: %s, "
              "stdout: %s, stderr: %s",
            command, exit-code, signal, child, stdout, stderr);
  block ()
    if (exit-code ~= 0)
      log-error("CGI failed to launch: %s, exit-code: %s, signal: %s",
                command, exit-code, signal);
    else
      write(output-stream(current-response()), read-to-end(stdout));
      write(output-stream(current-response()), read-to-end(stderr));
    end;
  cleanup
    log-debug("CGI waiting: %s", command);
    let (exit-code, signal) = wait-for-application-process(child);
    log-debug("CGI terminated: %s, exit-code: %s, signal: %s",
              command, exit-code, signal);
  end;
end method cgi-script-responder;

define method make-cgi-environment
    (script :: <locator>)
 => (environment :: <string-table>)
  let request :: <request> = current-request();
  let env :: <string-table> = make(<string-table>);

  // Values are stored in env in the order they appear in RFC 3875...

  let authentication = get-header(request, "Authentication", parsed: #t);
  if (authentication)
    env["AUTH_TYPE"] := first(authentication);
  end;

  let content-length = get-header(request, "Content-Length", parsed: #t);
  if (content-length & content-length > 0)
    env["CONTENT_LENGTH"] := content-length;
  end;

  let content-type = get-header(request, "Content-Type", parsed: #f);
  if (content-type)
    env["CONTENT_TYPE"] := content-type;
  end;

  env["GATEWAY_INTERFACE"] := "CGI/1.1";

  let url-tail = request.request-tail-url;
  if (url-tail)
    env["PATH_INFO"] := build-path(url-tail);
    env["PATH_TRANSLATED"]
      := as(<string>, merge-locators(as(<file-locator>, build-uri(url-tail)),
                                     *virtual-host*.document-root));
  end;

  env["REMOTE_HOST"] := request.request-client.client-listener.listener-host;

  // TODO: this is incorrect if there are multiple network interfaces.
  env["REMOTE_ADDR"] := $local-host.host-address;
                        // The listener doesn't know its address yet...
                        //request.request-client.client-listener
                        //       .listener-address.numeric-host-address;

  // Not supported: REMOTE_IDENT

  // Not supported: REMOTE_USER

  env["REQUEST_METHOD"] := as-uppercase(as(<string>, request.request-method));
  env["SCRIPT_NAME"] := as(<string>, script);
  env["SERVER_NAME"] := request.request-host;
  env["SERVER_PORT"]
    := integer-to-string(request.request-client.client-listener.listener-port);
  env["SERVER_PROTOCOL"] := "HTTP/1.1";
  env["SERVER_SOFTWARE"] := request.request-server.server-header;
  
  // TODO: Include some of the request headers, prepended with HTTP_.

  env
end method make-cgi-environment;
