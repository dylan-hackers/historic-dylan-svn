Module:    adobe-svg-example
Synopsis:  Example of using the <adobe-svg-gadget>.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define frame <svg-example-frame> (<simple-frame>)
  pane url-pane (frame)
    make(<text-field>, 
         value: "file://c|/windows/system32/adobe/svg viewer/svgabout.svg");

  pane load-button (frame)
    make(<push-button>, 
         label: "Load", 
         activate-callback: method(gadget)
                              on-load-button(frame)
                            end);

  pane svg-pane (frame)
    make(<adobe-svg-gadget>);

  layout (frame)
    vertically()
      horizontally()
        frame.load-button;
        frame.url-pane;
      end horizontally;
      frame.svg-pane;
    end vertically;

  keyword title: = "Adobe SVG Example";
end frame <svg-example-frame>;

define method on-load-button(frame :: <svg-example-frame>) => ()
  let svg = frame.svg-pane.atl-activex-interface;
  block()
    ISvgCtl/setSrc(svg, frame.url-pane.gadget-value);
  cleanup
    release(svg);
  end;
end method on-load-button;

define method main () => ()
  adobe-svg-gadget-initialize();
  block()
    start-frame(make(<svg-example-frame>));
  cleanup
    adobe-svg-gadget-uninitialize(); 
  end;
end method main;

begin
  main();
end;
