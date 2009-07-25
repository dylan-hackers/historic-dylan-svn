Module: httpi
Synopsis:  CGI script handling
Author:    Carl Gay
Copyright: Copyright (c) 2009 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define method cgi-script-responder
    (script :: <locator>)
  let command = as(<string>, script);
  output("cgi-script-responder");
end method cgi-script-responder;