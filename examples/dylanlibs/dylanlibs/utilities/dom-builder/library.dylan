Module:    dylan-user
Synopsis:  Utilities for generating Document Object Models for XML/HTML documents.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library dom-builder
  use common-dylan;
  use io;

  export 
    dom-builder, 
    simple-dom, 
    html-generator;
end library dom-builder;
