Module:    dylan-user
Synopsis:  Example of using the NNTP library
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library nntp-get-message
  use functional-dylan;
  use io;
  use duim;
  use deuce;
  use duim-deuce;
  use nntp;

  export nntp-get-message;
end library nntp-get-message;
