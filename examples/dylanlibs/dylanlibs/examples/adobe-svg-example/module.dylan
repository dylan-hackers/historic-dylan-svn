Module:    dylan-user
Synopsis:  Example of using the <adobe-svg-gadget>.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module adobe-svg-example
  use functional-dylan;
  use adobe-svg-dispatch;
  use adobe-svg-gadget;
  use duim, exclude: { make-node };
  use atl-activex-gadget;
  use com;
end module adobe-svg-example;
