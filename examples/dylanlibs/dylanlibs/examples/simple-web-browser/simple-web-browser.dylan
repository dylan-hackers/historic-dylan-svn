Module:    simple-web-browser
Synopsis:  Building a browser using the ATL ActiveX support
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

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


define frame <atl-browser-frame> (<simple-frame>)
  pane browser-pane (frame)
    make(<msie-gadget>);

  pane go-button (frame)
    make(<push-button>, label: "Go", activate-callback: on-go-button);

  pane url-pane (frame)
    make(<text-field>, text: "http://www.double.co.nz/dylan");

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
  let-com-interface html-control = browser-gadget.atl-activex-interface;
  let-resource url :: <bstr> = as(<bstr>, frame.url-pane.gadget-value);
  let-resource optional :: <lpvariant> = make(<lpvariant>);
  IWebBrowser2/Navigate(html-control, url, optional, optional, optional, optional);    
end method on-go-button;

define method main () => ()
  msie-gadget-initialize();
  start-frame(make(<atl-browser-frame>));
  msie-gadget-uninitialize();
end method main;

begin
  main();
end;
