Module:    httpi
Author:    Carl Gay
Synopsis:  An API for generating responses to HTTP requests.
Copyright: Copyright (c) 2001-2002 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Exported
//
define open primary class <response> (<base-http-response>)

  // The output stream is created lazily so that the user has the opportunity to
  // set properties such as stream type (e.g., binary or text) and buffering
  // characteristics.
  // @see output-stream
  slot %output-stream :: false-or(<stream>) = #f;

  slot headers-sent? :: <boolean> = #f;

  // Whether or not this is a buffered response.
  // @see output-stream
  constant slot response-buffered? :: <boolean> = #t;

end class <response>;

// Exported
//
define method add-header
    (response :: <response>, header :: <byte-string>, value :: <object>,
     #key if-exists? = #"replace")
  if (headers-sent?(response))
    raise(<koala-api-error>,
          "Attempt to add a %s header after headers have already been sent.",
          header);
  else
    next-method()
  end;
end method add-header;

// Exported
//
// ---TODO: The first time the output stream is requested, check the content
//          type and create the appropriate type of stream.
define method output-stream
    (response :: <response>) => (stream :: <stream>)
  response.%output-stream
  | if (response-buffered?(response))
      // If no virtual host then this is an error response.
      if (*virtual-host*)
        // The user can override this if they do it before writing to the
        // output stream.
        add-header(response, "Content-Type",
                   default-dynamic-content-type(*virtual-host*),
                   if-exists?: #"ignore");
      end;
      response.%output-stream := make(<string-stream>, direction: #"output");
    else
      signal(make(<koala-error>,
                  format-string: "Unbuffered responses aren't supported yet."));
    end
end;

// Exported
//
define method clear-output
    (response :: <response>) => ()
  let out = response.%output-stream;
  out & stream-contents(out, clear-contents?: #t)
end;

// The caller is telling us that either the request is complete or it's OK to
// send a partial response.  Send the header lines, whatever part of the body
// has been generated so far, and then clear the output stream.
//---*** Not sure this is legal HTTP.
//define method force-output
//    (response :: <response>) => ()
//  send-response(response);
//end;

define method send-header
    (stream :: <stream>, name :: <string>, val :: <pair>)
  for (v in val)
    send-header(stream, name, v)
  end;
end;

define method send-header
    (stream :: <stream>, name :: <string>, val :: <object>)
  format(stream, "%s: %s\r\n", name, val);
  log-copious("-->%s: %s", name, val);
end;

define method send-headers
    (response :: <response>, stream :: <stream>)
  // Send the headers
  let headers :: <header-table> = raw-headers(response);
  for (val keyed-by name in headers)
    send-header(stream, name, val);
  end;
  write(stream, "\r\n");  // blank line separates headers from body
  headers-sent?(response) := #t;
end;

// Send a response back to the client.  This is used for sending error
// responses as well as normal responses.  For error responses we can't
// assume there was a valid request.
//
define method send-response
    (response :: <response>) => ()
  let stream :: <stream> = request-socket(response-request(response));
  let req :: <request> = response-request(response);
  unless (headers-sent?(response))
    // Send the response line
    let response-line = format-to-string("%s %d %s\r\n",
                                         $http-version, 
                                         response.response-code, 
                                         response.response-reason-phrase | "OK");
    unless (req.request-version == #"HTTP/0.9")
      log-copious("-->%s", response-line);
      write(stream, response-line);
    end;

    // *virtual-host* may be #f if the request was invalid and
    // we're sending an error response.
    if (*virtual-host* & generate-server-header?(*virtual-host*))
      add-header(response, "Server", $server-header-value);
    end if;
    add-header(response, "Date", as-rfc1123-string(current-date()));

    let content-length :: <byte-string> = "0";
    unless (response.response-code == $not-modified)
      content-length := integer-to-string(stream-size(output-stream(response)));
      add-header(response, "Content-Length", content-length);
    end;
    unless (req.request-version == #"HTTP/0.9")
      send-headers(response, stream);
    end;
    // If sending an error response vhost may be #f, in which case we
    // have no log target.
    if (*virtual-host*)
      log-request(req, response.response-code, content-length);
    end;
  end unless; // headers already sent

  let contents = stream-contents(output-stream(response), clear-contents?: #t);
  unless (req.request-method == #"HEAD")
    // Send the body (or what there is of it so far).
    write(stream, contents);
  end;
end method send-response;

define inline function log-request
    (req :: <request>, response-code :: <integer>, content-length :: <string>)
  // Log in Common Logfile Format
  // (http://www.w3.org/Daemon/User/Config/Logging.html)
  let request = concatenate(as-uppercase(as(<string>, request-method(req))),
                            " ",
                            // Can happen e.g. when client sends no data.
                            request-raw-url-string(req) | "-",
                            " ",
                            as-uppercase(as(<string>, req.request-version)));
  let date = as-common-logfile-date(current-date());
  let remoteaddr = host-address(remote-host(request-socket(req)));

  // TODO: make the logfile format configurable.  e.g., the user
  //       specifies a string like this:
  //   "{ip} {hostname} [{date}] '{url}' {user-agent} {referer}"
  // See bug #7200.

  let log-entry
    = concatenate(remoteaddr, " ",
                  "-", " ",
                  "-", " ",
                  "[", date, "] ",
                  "\"", request, "\" ",
                  integer-to-string(response-code), " ",
                  content-length,
                  // for now, add User-Agent and Referer
                  " \"", as(<string>, get-header(req, "referer") | "-"),
                  "\" \"", as(<string>, get-header(req, "user-agent") | "-"),
                  "\"");
  log-raw(activity-log-target(*virtual-host*), log-entry);
end function log-request;

// Exported
// Convenience.  Seems common to want to add a numeric cookie value.
//
define method add-cookie
    (response :: <response>, name :: <string>, value :: <integer>, #rest args,
     #key max-age, path, domain, comment)
  apply(add-cookie, response, name, integer-to-string(value), args)
end;

// Exported
// This isn't the right way to handle cookies, but it's simple for now.
// ---TODO: Verify that comment is a TOKEN or QUOTED-STRING, and that other values are TOKENs.
//          See RFC 2109.
//
define method add-cookie
    (response :: <response>, name :: <string>, value :: <string>,
     #key max-age, path, domain, comment)
  add-header(response, "Set-cookie",
             with-output-to-string (s)
               format(s, "%s=%s; Version=%s", name, value, $default-cookie-version);
               max-age & format(s, "; Max-age=%s", max-age);
               path    & format(s, "; Path=%s", path);
               domain  & format(s, "; Domain=%s", domain);
               comment & format(s, "; Comment=\"%s\"", comment);
             end);
end method add-cookie;

