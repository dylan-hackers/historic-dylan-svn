Module:    dylan-user
Synopsis:  An XML file viewer using the MSXML libraries.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library msxml-viewer
  use functional-dylan;
  use io;
  use duim;
  use msxml3;

  // Add any more module exports here.
  export msxml-viewer;
end library msxml-viewer;
