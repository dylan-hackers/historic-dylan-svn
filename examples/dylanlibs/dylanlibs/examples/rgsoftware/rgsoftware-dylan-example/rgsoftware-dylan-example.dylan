Module:    rgsoftware-dylan-example
Synopsis:  Dylan version of RG Software's NN50 example
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// The main window for the example project.
define frame <example-frame> (<simple-frame>)
  pane status-pane (frame)
    make(<text-editor>, read-only?: #t);

  pane rms-pane (frame)
    make(<drawing-pane>, background: $black);

  layout (frame)
    vertically()
      make(<tab-control>,
           pages: vector(
                    make(<tab-control-page>, 
                         label: "Open Data File",
                         child: make(<open-data-file-pane>)),
                    make(<tab-control-page>, 
                         label: "(Optional) Load Trained Network",
                         child: make(<load-network-pane>)),
                    make(<tab-control-page>, 
                         label: "Train Network",
                         child: make(<train-network-pane>)),
                    make(<tab-control-page>, 
                         label: "Predict",
                         child: make(<predict-pane>)),
                    make(<tab-control-page>, 
                         label: "Input Relevance",
                         child: make(<input-relevance-pane>)),
                    make(<tab-control-page>, 
                         label: "About",
                         child: make(<about-pane>))));
      horizontally()
        vertically()
          make(<label>, label: "Status:");
          frame.status-pane;
        end vertically;
        vertically()
          make(<label>, label: "RMS Error:");
          frame.rms-pane;
        end vertically;
      end horizontally;
    end vertically;

  keyword title: = "Neural Network Example";
  keyword width: = 580;
end frame <example-frame>;

// A method that adds a single line to the status display
define method add-status-line( frame :: <example-frame>, line :: <string>) => ()
  frame.status-pane.gadget-value := concatenate(frame.status-pane.gadget-value, line, "\n");
end method add-status-line;

// Clear the status display
define method clear-status-display (frame :: <example-frame>) => ()
  frame.status-pane.gadget-value := "";
end method clear-status-display;

define method main () => ()
  start-frame(make(<example-frame>));
end method main;

begin
  main();
end;
