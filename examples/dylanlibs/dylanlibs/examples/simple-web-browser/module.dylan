Module:    dylan-user
Synopsis:  Building a browser using the ATL ActiveX support
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define module simple-web-browser
  use functional-dylan;
  use duim;
  use ms-internet;
  use com;
  use ole-automation;
  use atl-activex-gadget;
  use msie-gadget;

  // Add binding exports here.

end module simple-web-browser;
