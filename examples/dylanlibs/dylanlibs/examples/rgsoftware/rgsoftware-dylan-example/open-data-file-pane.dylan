Module:    rgsoftware-dylan-example
Synopsis:  Open Data File pane
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// The pane used for opening a data file
define pane <open-data-file-pane> ()
  // The field where the filename is entered
  pane filename-pane (pane)
    make(<text-field>, 
         text: "XOR.txt");

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

  // The button that opens the data file and loads it into our network object.
  pane open-button (pane)
    make(<push-button>, 
         label: "Open", 
         activate-callback: method(gadget) 
                              on-open-button(pane) 
                            end) ;

  // The helpful text describing this pane
  pane help-pane (pane)
    make(<text-editor>, 
         min-width: 350,
         min-height: 120,
         scroll-bars: #"dynamic",
         read-only?: #t, 
         text:
           "The very first step in running a neural network\n"
           "is to load your sample data. The data you load here will be\n"
           "used for training the neural network. It should consist of a\n"
           "few or more inputs and an expected output. An example would be\n"
           "the XOR file. When input #1 equals '1' and input #2 equals '2',\n"
           "the last column (or the 'expected output') equals '1'. And when\n"
           "both inputs are the same the expected output is '0'."); 

  // Layout the sub-panes within this pane.
  layout (pane)
    make(<group-box>, 
         label: "Step 1: Open Data File",
         child: vertically()
                  horizontally()
                    labelling("Filename") pane.filename-pane end;
                    pane.browse-button;
                    pane.open-button;
                  end horizontally;
                  pane.help-pane;
                end vertically);
end pane <open-data-file-pane>;

// Create a neural network compatible array from the data 
// contained within the given stream.
define function make-array-from-stream( stream :: <stream> ) => (array :: false-or(<array>))
  let lines = make(<stretchy-vector>);
  let result = #f;  

  for(line = read-line(stream, on-end-of-stream: #f)
      then read-line(stream, on-end-of-stream: #f),
      while: line)
    lines := add!(lines, map(string-to-float, split-by-comma(line)));
  end for;

  unless(empty?(lines))
    result := make(<array>, fill: 0d0, dimensions: list(lines.size, lines[0].size + 1));
    for(line keyed-by row in lines)
      for(value keyed-by column in line)
        result[row,column] := value;
      end for;
    end for;
  end unless;
    
  result;
end function make-array-from-stream;

// Open the data file and load the neural network.
define function on-open-button (pane)
  let filename = pane.filename-pane.gadget-value;
  block()

    with-open-file(fs = filename)
      *nn* := make(<neural-net>, array: make-array-from-stream(fs));
    end with-open-file;

    // Update status information
    let array = *nn*.neural-net-array;
    clear-status-display(pane.sheet-frame);
    for(a from 0 below array.dimensions[0])
      let status = "";
      for(b from 0 below array.dimensions[1])
        status := concatenate(status, tidy-float-to-string(array[a,b]), " ");
      end for;
      add-status-line(pane.sheet-frame, status);
    end for;
      
  exception(e :: <error>)
    notify-user("Could not open file.");
  end block;  
end function on-open-button;

