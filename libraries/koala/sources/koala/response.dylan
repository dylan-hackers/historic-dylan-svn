Module:    httpi
Author:    Carl Gay
Synopsis:  A response allows users to manipulate certain aspects of the response to the
           client, for example adding headers.
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define /*exported*/ open primary class <response> (<object>)
  constant slot get-request :: <request>, required-init-keyword: #"request";

  // The output stream is created lazily so that the user has the opportunity to
  // set the output stream type (e.g., binary or text).  See output-stream.
  slot response-output-stream :: false-or(<stream>) = #f;

  constant slot response-headers :: <header-table>, required-init-keyword: #"headers";
  slot headers-sent? :: <boolean> = #f;
  slot buffered? :: <boolean> = #t;
end;

define method initialize
    (response :: <response>, #key, #all-keys)
  next-method();
  when (*generate-server-header*)
    add-header(response, "Server", $server-header-value);
  end;
end;


define /*exported*/ method add-header
    (response :: <response>, header :: <string>, value :: <object>, #key if-exists? = #"append")
  if (headers-sent?(response))
    //---TODO: define a <api-error> class or something, and signal it here.
    error("Attempt to add a %s header after headers have already been sent.", header);
  else
    add-header(response.response-headers, header, value, if-exists?: if-exists?)
  end;
end;

define /*exported*/ method output-stream
    (response :: <response>) => (stream :: <stream>)
  response.response-output-stream
  | (response.response-output-stream := allocate-resource(<string-stream>));
end;

// This is guaranteed to be called as soon as the response is no longer in use.
// @see invoke-handler
define method deallocate-resources
    (response :: <response>) => ()
  let stream = response-output-stream(response);
  when (stream)
    deallocate-resource(<string-stream>, stream);
  end;
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
    (stream :: <tcp-socket>, name :: <string>, val :: <pair>)
  for (v in val)
    send-header(stream, name, v)
  end;
end;

define method send-header
    (stream :: <tcp-socket>, name :: <string>, val :: <object>)
  format(stream, "%s: %s\r\n", name, val);
  log-header("-->%s: %s", name, val);
end;

define method send-headers
    (response :: <response>, stream :: <tcp-socket>)
  // Send the headers
  let headers :: <header-table> = response-headers(response);
  for (val keyed-by name in headers)
    send-header(stream, name, val);
  end;
  write(stream, "\r\n");  // blank line separates headers from body
  headers-sent?(response) := #t;
end;

define method send-response
    (response :: <response>, #key response-code, response-message) => ()
  let stream :: <tcp-socket> = request-socket(get-request(response));
  unless (headers-sent?(response))
    // Send the response line
    format(stream, "%s %d %s\r\n",
           $http-version, response-code | 200, response-message | "OK");

    // Add required headers
    add-header(response, "Content-length", integer-to-string(stream-size(output-stream(response))));
    //---TODO: Remove this once set-response-stream-type (or whatever) is implemented.
    add-header(response, "Content-type", *default-dynamic-content-type*, if-exists?: #"do-nothing");

    send-headers(response, stream);
  end unless;

  // Send the body (or what there is of it so far).
  // Note that it's important for this to use clear-contents?: #f so that the
  // allocate-resource code can tell how big the underlying sequence is when it
  // tries to re-use the stream.
  let contents = stream-contents(output-stream(response), clear-contents?: #f);
  write(stream, contents);
end;

// Convenience.  Seems common to want to add a numeric cookie value.
//
define /*exported*/ method add-cookie
    (response :: <response>, name :: <string>, value :: <integer>, #rest args, #key)
  apply(add-cookie, response, name, integer-to-string(value), args)
end;

// This isn't the right way to handle cookies, but it's simple for now.
// ---TODO: Verify that comment is a TOKEN or QUOTED-STRING, and that other values are TOKENs.
//          See RFC 2109.
//
define /*exported*/ method add-cookie
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
end;
