Module:    rgsoftware-neural-net
Synopsis:  A wrapper around RG Software's Neural Net DLL
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// A high level neural net class that holds
// the current network array.
define class <neural-net> (<object>)
  // Array that holds the neural net data
  slot neural-net-array :: <array>, required-init-keyword: array:;
end class <neural-net>;

// Train the neural net with the data contained within
// the nets internal array.
//
// epoch-callback can be #f or a method that takes a
// single <integer> argument identifying the epoch nubmer.
// It should return #t to continue training the net or
// #f to stop processing.
//
// rms-callback can be #f or a method that takes a single
// <double> argument identifying the current RMS value. It
// returns no value.
define method train(nn :: <neural-net>, 
                    epochs :: <integer>,
                    learning-rate :: <double-float>,
                    momentum :: <double-float>,
                    max-neurons :: <double-float>,
                    #key 
                    epoch-callback :: false-or(<function>), 
                    rms-callback :: false-or(<function>), 
                    reset-weights? :: <boolean> = #t) => ()
  local method the-epoch-callback(x :: <integer>) => (y :: <integer>)
    if(epoch-callback)
      if(epoch-callback(x))
        1
      else
        0
      end if;
    else
      1
    end if;
  end method;

  local method the-rms-callback(x :: <double-float>) => (y :: <integer>)
    when(rms-callback)
      rms-callback(x);
    end when;
    1;
  end method;

  let (result, array) = 
    internal/train(nn.neural-net-array,
                   epochs,
                   if(reset-weights?) 1 else -1 end,
                   learning-rate,
                   momentum,
                   max-neurons,
                   the-epoch-callback,
                   the-rms-callback);
  if(result == -1)
    error("Unable to train neural network.");
  else
    nn.neural-net-array := array;
  end;                     
end method train;

// Return the percentage relevance of the inputs for the neural network.
define method input-relevance(nn :: <neural-net>,
                              epochs :: <integer>,
                              learning-rate :: <double-float>,
                              momentum :: <double-float>,
                              max-neurons :: <double-float>) 
  => (relevance :: <vector>)
  let relevance = make(<vector>, size: nn.neural-net-array.dimensions[1] - 2, fill: 0d0);
  let (result, nn-array, relevance-array) =
    internal/input-relevance(nn.neural-net-array,
                             relevance,
                             epochs,
                             learning-rate,
                             momentum,
                             max-neurons);
  if(result == -1)
    error("Unable to determine input relevance of neural network.");
  else
    nn.neural-net-array := nn-array;
    relevance-array;
  end if;  
end method input-relevance;

// Load a trained network from a file.
define method load-network(nn :: <neural-net>,
                           filename :: <string>,
                           learning-rate :: <double-float>,
                           momentum :: <double-float>,
                           max-neurons :: <double-float>) => ()
  let (result, array) = internal/load-network(nn.neural-net-array,
                                              filename,
                                              learning-rate,
                                              momentum,
                                              max-neurons);
  if(result == -1)
    error("Unable to load neural network.");
  else
    nn.neural-net-array := array;
  end if;
end method load-network;

// Save a network to a file
define method save-network(nn :: <neural-net>, filename :: <string>) => ()
  ignore(nn);
  if(internal/save-network(filename) == -1)
    error("Unable to save network.");
  end if;
end method save-network;
    
// Make a prediction based on the current state of the network.
// Requires a sequence of input values that will be used for
// predicting the output.
// Returns the output value.
define method predict(nn :: <neural-net>,  
                      inputs :: <sequence>,
                      learning-rate :: <double-float>,
                      momentum :: <double-float>,
                      max-neurons :: <double-float>) => (output :: <double-float>)
  let array :: <array> = nn.neural-net-array;
  let dim = array.dimensions;
  let prediction-array = make(<array>, fill: 0d0, dimensions: list(dim[0] + 1, dim[1]));
  for(x from 0 below dim[0])
    for(y from 0 below dim[1])
      prediction-array[x, y] := array[x, y]
    end for
  end for;
  
  for(input keyed-by x in inputs)
    prediction-array[dim[0], x] := input
  end for;

  internal/predict(prediction-array,
                   learning-rate,
                   momentum,
                   max-neurons);
end method predict;

// Stop any processes started using train or input-relevance
define method stop-processing(nn :: <neural-net>) => ()
  ignore(nn);
  internal/stop-processing();
end method stop-processing;

