Module:    atl-activex-gadget
Synopsis:  A gadget that hosts ActiveX controls by using the ATL DLL
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define C-function AtlAxWinInit
  result value :: <BOOL>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

// Holds the Module handle of the loaded ATL DLL
define variable *atl-module* = #f;

// Holds the registered windows address of the message
// used to retrieve the ActiveX interface.
define variable *get-control-message* = #f;

define function load-atl-library()
  unless(*atl-module*)
    *get-control-message* := RegisterWindowMessage("WM_ATLGETCONTROL");
    *atl-module* := LoadLibrary("atl.dll");
    let function = GetProcAddress(*atl-module*, "AtlAxWinInit");
    AtlAxWinInit(function);
  end;
end;

define function unload-atl-library() => ()
  when(*atl-module*)
    FreeLibrary(*atl-module*);
    *atl-module* := #f;
  end;
end;

// Must be called by the application before using this gadget.
// It is safe to call multiple times.
define function atl-activex-gadget-initialize() => ()
  load-atl-library();
end function atl-activex-gadget-initialize;

// Must be called by the application before exiting or when
// all instances of this gadget are destroyed.
// It is safe to call multiple times.
define function atl-activex-gadget-uninitialize() => ()
  unload-atl-library();
end function atl-activex-gadget-uninitialize;

// A gadget that can host ActiveX controls via the ATL
// library. Requires the latest version of the ATL runtime 
// which can be downloaded from Microsoft at:
//   http://activex.microsoft.com/controls/vc/atl.cab
define open abstract class <atl-activex-gadget> 
    (<win32-gadget-mixin>,
     <bordered-gadget-mixin>,
     <basic-value-gadget>,
     <leaf-pane>)
  // Returns the interface pointer for the control.
  virtual constant slot atl-activex-interface;
end class <atl-activex-gadget>;

// Must be implemented by derived class to return the class id
// of the ActiveX control being hosted.
define open generic atl-activex-class-id ( gadget :: <atl-activex-gadget> );

// Must be implemented by derived class to return the class used to
// represent the interface of the ActiveX control being hosted.
define open generic atl-activex-interface-class (gadget :: <atl-activex-gadget> );

define method atl-activex-interface( gadget :: <atl-activex-gadget> )
  let interface = c-type-cast(<c-interface>,
                              SendMessage(gadget.window-handle, 
                                *get-control-message*, 0, 0));
  let (result, com-interface) = QueryInterface(interface, 
    dispatch-client-uuid(atl-activex-interface-class(gadget)));  
  if(FAILED?(result))
    #f
  else
    pointer-cast(atl-activex-interface-class(gadget), com-interface);
  end;
end method atl-activex-interface;


define method class-for-make-pane(
  framem :: <win32-frame-manager>,
  class == <atl-activex-gadget>,
  #key)
=> (class :: <class>, options :: false-or(<sequence>))
  values(<atl-activex-gadget>, #f)
end method class-for-make-pane;

define method make-gadget-control(
  gadget :: <atl-activex-gadget>,
  parent :: <hwnd>,
  options,
  #key x, y, width, height)
=> (handle :: <HWND>)
  let ext-style = if(gadget.border-type == #"none") 0 else $WS-EX-CLIENTEDGE end;
  let handle :: <hwnd> = CreateWindowEx(ext-style,
                                        "AtlAxWin",
                                        as(<string>, atl-activex-class-id(gadget)),
                                        %logior(options,
                                                $WS-GROUP,
                                                $WS-TABSTOP),
                                        x, y, width, height,
                                        parent,
                                        $null-hMenu,
                                        application-instance-handle(),
                                        $NULL-VOID);
  handle
end method make-gadget-control;

