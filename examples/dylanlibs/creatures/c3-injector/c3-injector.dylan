Module:    c3-injector
Synopsis:  CAOS Injector for Creatures Adventures
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define frame <ca-injector-frame> (<simple-frame>)
  pane input-pane (frame)
    make(<text-editor>);

  pane inject-pane (frame)
    make(<push-button>, label: "Inject", activate-callback: on-inject);

  pane result-pane (frame)
    make(<rich-text-gadget>);

  pane about-pane (frame)
    make(<push-button>, label: "About", activate-callback: on-about);

  layout (frame)
    vertically()
      frame.input-pane;
      horizontally()
        frame.inject-pane;
        frame.about-pane;
      end;
      frame.result-pane;
    end;

  keyword title: = "CAOS Injector for Creatures 3";
  keyword width: = 400;
  keyword height: = 250;
end frame <ca-injector-frame>;

define method on-inject(gadget)
  with-busy-cursor( gadget.sheet-frame )
    let result = raw-docker-caos(gadget.sheet-frame.input-pane.gadget-value);
    gadget.sheet-frame.result-pane.gadget-value := result;
  end;
end method on-inject;

define method on-about(gadget)
  notify-user(concatenate("Version 1.3\nCopyright (c) 1999, Chris Double.\n"
              "All Rights Reserved.\n"
              "Using c3-engine.dll V", c3-engine-version(), "\n",                          
              "Using double-rich-text-editor.dll V", rich-text-gadget-version(), "\n",                          
              "http://www.double.co.nz/creatures"),
              title: "Creatures 3 CAOS Injector");
end method;

define method main () => ()
  start-frame(make(<ca-injector-frame>));
end method main;

begin
  main();
end;
