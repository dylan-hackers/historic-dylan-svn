Module:    base64-example
Synopsis:  Example of using the base64 library.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define frame <base64-example> (<simple-frame>)
  pane text-input-pane (frame)
    make(<text-editor>);

  pane base64-text-output-pane (frame)
    make(<text-editor>, read-only?: #t);

  pane decoded-output-pane (frame)
    make(<text-editor>, read-only?: #t);

  pane convert-button (frame)
    make(<push-button>, 
         label: "Convert Text", 
         activate-callback: method(g) on-convert-button(g.sheet-frame) end);

  layout (frame)
    vertically()
      frame.convert-button;
      frame.text-input-pane;      
      frame.base64-text-output-pane;
      frame.decoded-output-pane;
    end;

  keyword title: = "Base64 Example";
end frame <base64-example>;

define method on-convert-button(frame :: <base64-example>) => ()
  let b64 = base64-encode(frame.text-input-pane.gadget-value);
  frame.base64-text-output-pane.gadget-value := b64.base64-string;
  frame.decoded-output-pane.gadget-value := base64-decode-as(<string>, b64);
end method on-convert-button;

define method main () => ()
  start-frame(make(<base64-example>));
end method main;

begin
  main();
end;
