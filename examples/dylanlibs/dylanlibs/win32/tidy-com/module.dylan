Module:    dylan-user
Synopsis:  Wrapper to TidyCOM component
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define module tidy-com
  use c-ffi;
  use ole-automation;
  use functional-dylan;
  use type-library-module, export: all;


  // Add binding exports here.

end module tidy-com;

define module html-tidy
  use functional-dylan;
  use tidy-com;
  use ole-automation;
  use threads, exclude: { release };

  
  export html-tidy;  
end module html-tidy;
