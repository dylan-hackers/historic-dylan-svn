Module:    msie-gadget
Synopsis:  A gadget that displays an instance of Microsoft Internet Explorer
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <msie-gadget> (<atl-activex-gadget>)
end class <msie-gadget>;

define method atl-activex-class-id( msie :: <msie-gadget>) 
    $WebBrowser-class-id
end method atl-activex-class-id;

define method atl-activex-interface-class (msie :: <msie-gadget>)
  <IWebBrowser2>
end method atl-activex-interface-class;

define method class-for-make-pane(
  framem :: <win32-frame-manager>,
  class == <msie-gadget>,
  #key)
=> (class :: <class>, options :: false-or(<sequence>))
  values(<msie-gadget>, #f)
end method class-for-make-pane;

define function msie-gadget-initialize() => ()
  atl-activex-gadget-initialize();
end function msie-gadget-initialize;

define function msie-gadget-uninitialize() => ()
  atl-activex-gadget-uninitialize();
end function msie-gadget-uninitialize;
