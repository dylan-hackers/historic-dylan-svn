Module:    httpi
Synopsis:  HTTP Support
Author:    Gail Zacharias
Copyright: Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define inline function current-url () => (url :: <url>);
  *request*.request-url
end;

define open generic redirect-to (object :: <object>);

define method redirect-to (url :: <string>)
  let headers = current-response().response-headers;
  add-header(headers, "Location", url);
  see-other-redirect(headers: headers);
end method redirect-to;

define method redirect-to (url :: <url>)
  redirect-to(build-uri(url));
end;
