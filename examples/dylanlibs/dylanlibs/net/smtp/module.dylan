Module:    dylan-user
Synopsis:  SMTP mail interface
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module smtp
  use functional-dylan;
  use format;
  use streams;
  use sockets;

  export 
   initialize-smtp-library,
   send-mail;
end module smtp;
