Module:    rgsoftware-dylan-example
Synopsis:  Input Relevance pane
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define pane <input-relevance-pane> ()
  // The button that starts the input relevance display.
  pane input-relevance-button (pane)
    make(<push-button>, 
         label: "Input Relevance", 
         activate-callback: method(gadget) 
                              on-input-relevance-button(pane) 
                            end) ;

  // The helpful text describing this pane
  pane help-pane (pane)
    make(<text-editor>, 
         min-width: 200,
         min-height: 150,
         scroll-bars: #"dynamic",
         read-only?: #t, 
         text:
           "This step will tell you how\n"
           "important each input is, or how\n"
           "well it's correlated to the output.\n"
           "The values are expressed in\n"
           "percentages and displayed in\n"
           "the lower left 'Status' text box."); 

  // Layout the sub-panes within this pane.
  layout (pane)
    make(<group-box>, 
         label: "Step 5: Input Relevance",
         child: vertically()
                  pane.help-pane;
                  pane.input-relevance-button;
                end vertically);
end pane <input-relevance-pane>;

// Perform the Input Relevance and display to user.
define function on-input-relevance-button(pane)
  unless(*nn*)
    notify-user("You must load a network first!");
  end unless;

  let epochs = choose-integer(title: "Number of Epochs", owner: pane.sheet-frame);
  when(epochs)
    block()
      clear-status-display(pane.sheet-frame);
      let result = input-relevance(*nn*, epochs, *learning-rate*, *momentum*, *max-neurons*);
      for(relevance keyed-by column in result)
        let status = format-to-string("Input relevance for column %d: %s%%",
                                      column + 1,
                                      tidy-float-to-string(relevance));
        add-status-line(pane.sheet-frame, status);
      end for;
    exception(e :: <error>)
      notify-user("Could not make classification!");
    end block;     
  end when;
end function on-input-relevance-button;

