Module:    dylan-user
Synopsis:  An example of using http-client library to send SMS messages via a web form.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library http-client-example
  use functional-dylan;
  use io;
  use duim;
  use http-client;
  use sequence-utilities;
end library http-client-example;
