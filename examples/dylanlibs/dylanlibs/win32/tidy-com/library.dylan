Module:    dylan-user
Synopsis:  Wrapper to TidyCOM component
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define library tidy-com
  use c-ffi;
  use ole-automation;
  use functional-dylan;

  // Add any more module exports here.
  export tidy-com,html-tidy;
end library tidy-com;
