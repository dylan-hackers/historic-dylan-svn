Module:    simple-web-browser
Synopsis:  Building a browser using the ATL ActiveX support
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define C-function AtlAxWinInit
  result value :: <BOOL>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define variable *atl-module* = #f;
define variable *get-control-message* = #f;

define function load-atl-library()
  *atl-module* := LoadLibrary("atl.dll");
  let function = GetProcAddress(*atl-module*, "AtlAxWinInit");
  AtlAxWinInit(function);
  *get-control-message* := RegisterWindowMessage("WM_ATLGETCONTROL");
end;

define function unload-atl-library() => ()
  FreeLibrary(*atl-module*);
  *atl-module* := #f;
end;

define macro let-com-interface
  { let-com-interface ?:name :: ?type:expression = ?value:expression;
    ?:body }
  => { let ?name :: ?type = ?value;
       block()         
         ?body
       cleanup
         release(?name)
       end }
end macro let-com-interface;

define macro let-resource
  { let-resource ?:name :: ?type:expression = ?value:expression;
    ?:body }
  => { let ?name :: ?type = ?value;
       block()         
         ?body
       cleanup
         destroy(?name)
       end }
end macro let-resource;

define class <html-gadget> (<value-gadget>)
end class <html-gadget>;

define class <msie-html-gadget>
  (<win32-gadget-mixin>,
   <bordered-gadget-mixin>,
   <html-gadget>,
   <basic-value-gadget>,
   <leaf-pane>)
  virtual constant slot msie-ole-control;
end class <msie-html-gadget>;

define method msie-ole-control( gadget :: <msie-html-gadget> )
  let interface = c-type-cast(<c-interface>,
                              SendMessage(gadget.window-handle, 
                                *get-control-message*, 0, 0));
  let (result, com-interface) = QueryInterface(interface, 
    as(<refiid>, "{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}"));  
  pointer-cast(<iwebbrowser2>, com-interface);
end method msie-ole-control;

define method class-for-make-pane(
  framem :: <win32-frame-manager>,
  class == <html-gadget>,
  #key)
=> (class :: <class>, options :: false-or(<sequence>))
  values(<msie-html-gadget>, #f)
end method class-for-make-pane;

define method make-gadget-control(
  gadget :: <msie-html-gadget>,
  parent :: <hwnd>,
  options,
  #key x, y, width, height)
=> (handle :: <HWND>)
  let ext-style = if(gadget.border-type == #"none") 0 else $WS-EX-CLIENTEDGE end;
  let handle :: <hwnd> = CreateWindowEx(ext-style,
                                        "AtlAxWin",
                                        "http://www.double.co.nz",
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
                                                



define frame <atl-browser-frame> (<simple-frame>)
  pane browser-pane (frame)
    make(<html-gadget>);

  pane go-button (frame)
    make(<push-button>, label: "Go", activate-callback: on-go-button);

  pane url-pane (frame)
    make(<text-field>, text: "http://www.double.co.nz");

  layout (frame)
    vertically()
      horizontally()
        frame.url-pane;
        frame.go-button;
      end;
      frame.browser-pane;
    end;

  keyword title: = "Simple Web Browser";
end frame <atl-browser-frame>;

define method on-go-button(g)
  let frame = g.sheet-frame;
  let browser-gadget = frame.browser-pane;
  let-com-interface html-control = browser-gadget.msie-ole-control;
  let-resource url :: <bstr> = as(<bstr>, frame.url-pane.gadget-value);
  let-resource optional :: <lpvariant> = make(<lpvariant>);
  IWebBrowser2/Navigate(html-control, url, optional, optional, optional, optional);    
end method on-go-button;

define method main () => ()
  unless(load-atl-library())
    notify-user("Failed to load ATL library.");
  end;
  start-frame(make(<atl-browser-frame>));
  unload-atl-library();
end method main;

begin
  main();
end;
