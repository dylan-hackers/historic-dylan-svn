Module:    dylan-user
Synopsis:  Money arithmetic type
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define module money
  use functional-dylan;
  use print;
  use format;
  use streams;

  // Add binding exports here.
  export <money>, 
         make-money,
         money-dollars,
         money-cents,
         string-to-money,
         money-to-string,
         parse-money;
end module money;
