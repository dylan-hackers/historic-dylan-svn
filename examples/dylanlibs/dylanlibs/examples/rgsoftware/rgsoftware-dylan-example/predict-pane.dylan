Module:    rgsoftware-dylan-example
Synopsis:  Make a prediction from a loaded/trained neural net.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define pane <predict-pane> ()
  // Button to start prediction process
  pane predict-button (pane)
    make(<push-button>,
         label: "Predict",
         activate-callback: method(gadget) 
                              on-predict-button(pane) 
                            end);

  // The helpful text describing this pane
  pane help-pane (pane)
    make(<text-editor>, 
         min-width: 100,
         min-height: 20,
         scroll-bars: #"dynamic",
         read-only?: #t, 
         text:
           "Make a prediction!");

  // Layout the sub-panes within this pane.
  layout (pane)
    make(<group-box>, 
         label: "Step 4: Predict",
         child: horizontally()
                  pane.help-pane;
                  pane.predict-button;
                end horizontally);
end pane <predict-pane>;

define function on-predict-button(pane)
  let dim = *nn*.neural-net-array.dimensions;
  let inputs = make(<vector>, fill: 0d0, size: dim.second - 2);
  for(index from 0 below inputs.size)
    let value = choose-integer(title: format-to-string("Value for column %d", index + 1),
                               owner: pane.sheet-frame);
    inputs[index] := as(<double-float>, value);
  end for;

  let result = predict(*nn*, 
                       inputs, 
                       *learning-rate*,
                       *momentum*,
                       as(<double-float>, *max-neurons*));
  
  notify-user(format-to-string("Predicted Value: %s", tidy-float-to-string(result)));
end function on-predict-button;

