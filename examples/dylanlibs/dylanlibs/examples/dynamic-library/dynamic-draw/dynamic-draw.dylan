Module:    dynamic-draw
Synopsis:  Drawing program that loads Dylan functionality at run time
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <dynamic-drawing-pane> (<drawing-pane>)
end class <dynamic-drawing-pane>;

define frame <dynamic-draw-frame> (<simple-frame>)
  slot current-tool :: false-or(<dynamic-draw-tool>) = #f;

  pane choose-tool-button (frame)
    make(<push-button>, 
         label: "Tool...", 
         activate-callback: method(g) on-choose-tool(frame) end);

  pane load-tool-button (frame)
    make(<push-button>, 
         label: "Load Tool...",
         activate-callback: method(g) on-load-tool(frame) end);

  pane remove-tool-button (frame)
    make(<push-button>, 
         label: "Remove Tool...",
         activate-callback: method(g) on-remove-tool(frame) end);

  pane exit-button (frame)
    make(<push-button>,
         label: "Exit",
         activate-callback: method(g) exit-frame(frame) end);
    
  pane drawing-pane (frame)
    make(<dynamic-drawing-pane>);

  layout (frame)
    vertically()
      horizontally()
        frame.choose-tool-button;
        frame.load-tool-button;
        frame.remove-tool-button;
        frame.exit-button;
      end;
      frame.drawing-pane;
    end;

  keyword title: = "Dynamic Draw";
end frame <dynamic-draw-frame>;

define method handle-button-event(pane :: <dynamic-drawing-pane>, event :: <button-press-event>, button) => ()
  when(pane.sheet-frame.current-tool)
    with-sheet-medium(medium = pane)
      do-tool(pane.sheet-frame.current-tool, medium, event-x(event), event-y(event));
    end with-sheet-medium;
  end when;     
end method handle-button-event; 

define method on-choose-tool(frame :: <simple-frame>) => ()
  let tool = choose-from-dialog(all-tools(),
                                title: "Choose Tool...",
                                owner: frame,
                                label-key: tool-description);
  when(tool)
    frame.current-tool := tool
  end when;
end method on-choose-tool;

define method on-load-tool(frame :: <simple-frame>) => ()
  let file = choose-file(title: "Load Tool DLL", owner: frame);
  when(file)
    load-dynamic-library(file);
  end when;
end method on-load-tool;

define method on-remove-tool(frame :: <simple-frame>) => ()
  let choice = choose-from-dialog(dynamic-libraries(),
                                  title: "Remove Tool",
                                  owner: frame,
                                  label-key: dynamic-library-description);
  when(choice)
    frame.current-tool := #f;
    unload-dynamic-library(choice);
  end when;                                  
end method on-remove-tool;

// A simple non-dynamically loaded tool
define class <rectangle-tool> (<dynamic-draw-tool>)
end class <rectangle-tool>;

define method tool-description(tool :: <rectangle-tool>) => (d :: <string>)
  "Draw a rectangle";
end method tool-description;

define method do-tool(tool :: <rectangle-tool>, surface, x, y) => ()
  draw-rectangle(surface, x, y, x + 20, y + 20, filled?: #f);
end method do-tool;

define method main () => ()
  start-frame(make(<dynamic-draw-frame>));
end method main;

begin
  register-tool(make(<rectangle-tool>));  
  main();
end;
