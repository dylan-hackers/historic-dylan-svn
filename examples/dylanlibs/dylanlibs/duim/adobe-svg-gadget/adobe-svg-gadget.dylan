Module:    adobe-svg-gadget
Synopsis:  Gadget that hosts the Adobe SVG Control
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <adobe-svg-gadget> (<atl-activex-gadget>)
end class <adobe-svg-gadget>;

define method atl-activex-class-id( gadget :: <adobe-svg-gadget>) 
    $SVGCtl-class-id
end method atl-activex-class-id;

define method atl-activex-interface-class (gadget :: <adobe-svg-gadget>)
  <ISVGCtl>
end method atl-activex-interface-class;

define method class-for-make-pane(
  framem :: <win32-frame-manager>,
  class == <adobe-svg-gadget>,
  #key)
=> (class :: <class>, options :: false-or(<sequence>))
  values(<adobe-svg-gadget>, #f)
end method class-for-make-pane;

define function adobe-svg-gadget-initialize() => ()
  atl-activex-gadget-initialize();
end function adobe-svg-gadget-initialize;

define function adobe-svg-gadget-uninitialize() => ()
  atl-activex-gadget-uninitialize();
end function adobe-svg-gadget-uninitialize;
