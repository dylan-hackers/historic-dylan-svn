Module:    rgsoftware-dylan-example
Synopsis:  Train the neural network pane
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define pane <train-network-pane> ()
  // The field where the number of hidden neurons is entered
  pane number-neurons-pane (pane)
    make(<text-field>,
         value-type: <integer>,
         text: "50");

  // A checkbox for choosing whether to reset the weights
  pane reset-weights-pane (pane)
    make(<check-button>, label: "Reset Weights?", value: #t);

  // An entry field to enter the number of epochs
  pane epochs-pane (pane)
    make(<text-field>,
         value-type: <integer>,
         text: "5000");
  
  // The button that starts the training.
  pane train-button (pane)
    make(<push-button>, 
         label: "Train", 
         activate-callback: method(gadget) 
                              on-train-button(pane) 
                            end) ;

  // The helpful text describing this pane
  pane help-pane (pane)
    make(<text-editor>, 
         min-width: 330,
         min-height: 170,
         scroll-bars: #"dynamic",
         read-only?: #t, 
         text:
           "This is the most important part so don't skip it!\n"
           "You have to train your neural network to make it smart,\n"
           "otherwise you will get 'garbage' when predicting values\n"
           "(unless you've already loaded a previously trained neural\n"
           "network in Step 2). If you've already trained your neural\n"
           "network and want to start over, or if this is the first\n"
           "time training the neural network, check 'Reset weights'\n"
           "before training. The number of hidden neurons can effect\n"
           "the performance of the neural network. Its a general rule\n"
           "to use as few neurons as possible to achieve the desired results.\n"); 

  // Layout the sub-panes within this pane.
  layout (pane)
    make(<group-box>, 
         label: "Step 3: Train the Neural Network",
         child: horizontally()
                  pane.help-pane;
                  vertically(spacing: 5)
                    pane.reset-weights-pane;
                    make(<label>, label: "Number of Hidden Neurons");
                    pane.number-neurons-pane;
                    make(<label>, label: "Number of Epochs");
                    pane.epochs-pane;
                    pane.train-button;
                  end vertically;
                end horizontally);
end pane <train-network-pane>;

// Forward declare callback functions
define generic on-epoch(frame, epoch :: <integer>) => (r :: <boolean>);
define generic on-rms-error(frame, rmse :: <double-float>) => ();

define function on-train-button(pane)
  block()
    train(*nn*, 
          pane.epochs-pane.gadget-value,
          *learning-rate*,
          *momentum*,
          as(<double-float>, pane.number-neurons-pane.gadget-value),
          reset-weights?: pane.reset-weights-pane.gadget-value,
          epoch-callback: curry(on-epoch, pane.sheet-frame),
          rms-callback: curry(on-rms-error, pane.sheet-frame));

    // Update status pane
    clear-status-display(pane.sheet-frame);
    let array = *nn*.neural-net-array;
    for(a from 0 below array.dimensions[0])
      let status = format-to-string("Expected: %s Actual: %s",
                                    tidy-float-to-string(array[a, array.dimensions[1] - 2]),
                                    tidy-float-to-string(array[a, array.dimensions[1] - 1]));
      add-status-line(pane.sheet-frame, status);
    end for;

    // Check to see if user wants to save the network
    when(notify-user("Would you like to save this trained neural network?",
                      title: "Network Trained!",
                      style: #"question"))
      // Save the neural net
      let file = choose-file(direction: #"output");
      when(file)
        add-status-line(pane.sheet-frame, format-to-string("Saving %s...", file));
        save-network(*nn*, file);
      end when;
    end when;

//  exception(e :: <error>)
//    notify-user("Could not train network.");
  end block;
end function on-train-button;

// Callback functions for displaying/reporting training progress
define method on-rms-error(frame, rmse :: <double-float>) => ()
  *rms-error* := rmse;
end method on-rms-error;

// Variables to control chart refresh rate
define variable *refresh* = 0;
define constant $refresh-rate = 100;
define variable *last-rms-error* = 0d0;
define variable *last-epoch* = 0;

define method on-epoch(frame, epoch :: <integer> ) => (r :: <boolean>)
  if(epoch > 10000)
    notify-user("You can set the return of the epoch-callback function to\n"
                "#f to stop processing or use the stop-processing method.");
    #f
  else      
    let (width, height) = sheet-size(frame.rms-pane);
    let brush = $white;
    let pen = make(<pen>, width: 1);

    // Given an epoch, return the x position to plot
    local method epoch-to-x(epoch)
      (epoch / 5000d0) * width;
    end method;

    // Given an rms-error, return the y position to plot
    local method rms-to-y(rms)
      height - (rms * height)
    end method;

    *refresh* := *refresh* + 1;
    when(*refresh* == $refresh-rate)
      *refresh* := 0;
      frame.frame-title := format-to-string("Epoch: %d", epoch);
      with-sheet-medium(medium = frame.rms-pane)
        with-drawing-options(medium, brush: brush, pen: pen)
          draw-point(medium, epoch-to-x(*last-epoch*),  rms-to-y(*last-rms-error*));
          line-to(medium, epoch-to-x(epoch), rms-to-y(*rms-error*));

          *last-epoch* := epoch;
          *last-rms-error* := *rms-error*;
        end with-drawing-options;
      end with-sheet-medium;
    end when; 

    when(epoch == 1)
      with-sheet-medium(medium = frame.rms-pane)
        clear-box*(medium, $everywhere);
        with-drawing-options(medium, brush: brush, pen: pen)        
          draw-point(medium, epoch-to-x(epoch),  rms-to-y(*rms-error*));
          *last-epoch* := epoch;
          *last-rms-error* := *rms-error*;
        end with-drawing-options;
      end with-sheet-medium; 
    end when; 
    #t;
  end if;
end method on-epoch;

