Module:    rgsoftware-dylan-example
Synopsis:  Load a previously saved network.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define pane <load-network-pane> ()
  // The field where the filename is entered
  pane filename-pane (pane)
    make(<text-field>, 
         text: "c:\\network.net");

  // A browsing button allowing selection of a file.
  pane browse-button (pane)
    make(<push-button>, 
         label: "Browse...", 
         activate-callback: 
           method(gadget)
             let file = choose-file(direction: #"input");
             when(file)
               pane.filename-pane.gadget-value := file;
             end when;
           end method);

  // The button that opens the file and loads the network.
  pane load-button (pane)
    make(<push-button>, 
         label: "Load", 
         activate-callback: method(gadget) 
                              on-load-button(pane) 
                            end) ;

  // The helpful text describing this pane
  pane help-pane (pane)
    make(<text-editor>, 
         min-width: 200,
         min-height: 120,
         scroll-bars: #"dynamic",
         read-only?: #t, 
         text:
           "You can load a trained neural network and skip\n"
           "the training process in Step 3, but you must at\n"
           "least load the data in Step 1 before predicting\n"
           "values.");

  // Layout the sub-panes within this pane.
  layout (pane)
    make(<group-box>, 
         label: "Step 2: (optional) Load Trained Neural Network",
         child: vertically()
                  horizontally()
                    labelling("Filename") pane.filename-pane end;
                    pane.browse-button;
                    pane.load-button;
                  end horizontally;
                  pane.help-pane;
                end vertically);
end pane <load-network-pane>;

define function on-load-button(pane)
  let name = pane.filename-pane.gadget-value;
  unless(file-exists?(name))
    notify-user(format-to-string("Could not find file %s", name));
  end unless;

  clear-status-display(pane.sheet-frame);
  add-status-line(pane.sheet-frame, format-to-string("Loading %s...", name));

  block()
    with-busy-cursor(pane.sheet-frame)
      load-network(*nn*, 
                   name,
                   *learning-rate*,
                   *momentum*,
                   as(<double-float>, *max-neurons*));
    end with-busy-cursor;
  exception(e :: <error>)
    notify-user("Unable to load neural network.");
  end block;

  clear-status-display(pane.sheet-frame);
  add-status-line(pane.sheet-frame, "Network loaded");
end function on-load-button;

