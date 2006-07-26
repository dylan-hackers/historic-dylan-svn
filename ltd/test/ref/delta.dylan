//  File DeltaRule.Lisp
//  Copyright 1990 by Mark Wilson
//  Externally (callable) functions in this file: ;;;;;;;;;;;;;;;;;;;;;;
//  (NewDeltaNetwork sizeList)
//      Args:  sizeList = list of sizes of slabs. This also defines
//             the number of slabs in the network.
//             (e.g., '(10 5 4) ==> a 3-slab network with 10
//             input neurons, 5 hidden neurons, and 4 output
//             neurons).
// 
//   Returned value = a list describing the network:
//    (nLayers sizeList
//      (activation-array[l] .. activation-array[nLayers])
//      (weight-array[2] .. weight-array[nLayers])
//      (sum-of-products[2] .. sum-of-products[nLayers[nLayers])
//      (back-prop-error[2] .. back-prop-error[nLayers]))
//      (old-delta-weights[2] .. for momentum term
// (DeltaLearn networkList trainingList)
//   Args:  networkList = list returned from function NewDeltaNetwork
//         trainingList = a list of lists of trainlng exemplars.
//              For example, a list might be:
//              (((0 1) (1 0))  ; first exemplar
//               ((1 0) (0 1))) ; second exemplar
//              Note: the inner sub-lists can also be arrays.
//      nlterations = number of complete training iterations
// 
//   Returned value = average error at output neurons for last
//         training cycle.
//  (DeltaRecall networkList inputList)
//   Args:  networkList = list returned from function 'NewDeltaNetwork'
//      inputList = list OR array of input activation values
// 
//   Returned value = list of output neuron values
//  (DeltaPlot networkList) ==> plots a network. Must call '(init-plot) first.
//  (WriteDeltaNetwork fileName networkList) ==> saves a network to disk
//  (ReadDeltaNetwork fileName) ==> returns a network list
//  End of list of externally callable functions ;;;;;;;;;;;;;;;;
#f;

//  Define default learning rates for each layer of neurons:
defaulteidalist := #(0.5, 0.4, 0.3, 0.2, 0.08, 0.07);

//  Define the default noise to add to each input neuron:
*delta-default-input-noise-value* := 0.08;

*delta-rule-debug-flag* := #f;

// 
//  Create a new delta network:                
// 
//  alpha = coefficient for new weight change
//  beta = coefficient for adding in last weight change
define method newdeltanetwork (sizelist, #key alpha = 0.2, beta = 0.8)
  let numlayers = size(sizelist);
  let w-list = #f;
  let dw-list = #f;
  let old-dw-list = #f;
  let a-list = #f;
  let s-list = #f;
  let d-list = #f;
  //  back propagated deltas
  eidalist := defaulteidalist;
  //         
  //  Initialize storage for activation energy for all slabs:
  //
  a-list
   := map(method (size) make(<array>, dimensions: list(size)); end method,
          sizelist);
  // 
  //  Initialize storage for sum of products arrays:
  //
  s-list
   := map(method (size) make(<array>, dimensions: list(size)); end method,
          tail(sizelist));
  // 
  //  Initialize storage for delta arrays:
  //
  d-list
   := map(method (size) make(<array>, dimensions: list(size)); end method,
          tail(sizelist));
  // 
  //  Initialize storage for the weights:
  //
  for (i from 0 below numlayers - 1)
    w-list := pair(list(sizelist[i], sizelist[i + 1]), w-list);
  end for;
  w-list
   := map(method (size) make(<array>, dimensions: size); end method,
          reverse(w-list));
  // 
  //  Initialize the storage for delta weights;
  //
  for (i from 0 below numlayers - 1)
    dw-list := pair(list(sizelist[i], sizelist[i + 1]), dw-list);
  end for;
  dw-list
   := map(method (size) make(<array>, dimensions: size); end method,
          reverse(dw-list));
  // 
  //  Initialize the storage for old delta weights:
  //
  for (i from 0 below numlayers - 1)
    old-dw-list := pair(list(sizelist[i], sizelist[i + 1]), old-dw-list);
  end for;
  old-dw-list
   := map(method (size) make(<array>, dimensions: size); end method,
          reverse(old-dw-list));
  // 
  //  Initialize values for all activations:
  //
  begin
    do(method (x)
         let num = dimension(x, 0);
         for (n from 0 below num) x[n] := frandom(0.01, 0.1); end for;
       end method,
       a-list);
    a-list;
  end;
  // 
  //  Initialize values for all weights:
  //
  begin
    do(method (x)
         let numi = dimension(x, 0);
         let numj = dimension(x, 1);
         for (j from 0 below numj)
           for (i from 0 below numi)
             begin x[i, j] := frandom; -0.5 := 0.5; end;
           end for;
         end for;
       end method,
       #f);
    #f;
  end;
  w-list;
  list(numlayers, sizelist, a-list, s-list, w-list, dw-list, d-list,
       old-dw-list, alpha, beta);
end method newdeltanetwork;

// 
//  Utility function for training a delta rule neural network.
//  The first argument is a network definition (as returned from
//  NewDeltaNetwork), the second argument is a list of training
//  data cases (see the example test functions at the end of this
//  file for examples), the third (optional) argument is a flag
//  for automatic plotting of the state of the network (activated
//  by holding down the mouse button while this function is
//  executing, and the fourth (optional) argument is formatting
//  information passed to function DeltaPlot to specify plotting
//  the input neuron activation values as a two dimensional array
//  instead of the (default) format of a one dimensional array.
//
define method deltalearn (netlist, trainlist, #key autoplot = #"no",
                          input-format = #f)
  let nlayers = head(netlist);
  let sizelist = second(netlist);
  let activationlist = third(netlist);
  let sumofproductslist = head(cdddr(netlist));
  let weightlist = second(cdddr(netlist));
  let deltaweightlist = third(cdddr(netlist));
  let deltalist = cadddr(cdddr(netlist));
  let olddeltaweightlist = cadddr(cdddr(tail(netlist)));
  let alpha = cadddr(cdddr(tail(tail(netlist))));
  let beta = cadddr(cdddr(cdddr(netlist)));
  let inputs = #f;
  let targetoutputs = #f;
  let idimension = #f;
  let jdimension = #f;
  let iactivationvector = #f;
  let jactivationvector = #f;
  let n = #f;
  let weightarray = #f;
  let sumofproductsarray = #f;
  let ideltavector = #f;
  let jdeltavector = #f;
  let deltaweightarray = #f;
  let olddeltaweightarray = #f;
  let sum = #f;
  let isumofproductsarray = #f;
  let error = #f;
  let outputerror = 0;
  let delta = #f;
  let eida = #f;
  let inputnoise = 0;
  // 
  //  Zero out deltas:
  //
  for (n from 0 below nlayers - 1)
    let dw = deltalist[n];
    let len1 = dimension(dw, 0);
    for (i from 0 below len1) dw[i] := 0; end for;
  end for;
  // 
  //  Zero out delta weights:
  //
  for (n from 0 below nlayers - 1)
    let dw = deltaweightlist[n];
    let len1 = dimension(dw, 0);
    let len2 = dimension(dw, 1);
    for (i from 0 below len1)
      for (j from 0 below len2) dw[i, j] := 0; end for;
    end for;
  end for;
  inputnoise := *delta-default-input-noise-value*;
  // 
  //  Main loop on training examples:
  //
  for (tl in trainlist)
    inputs := head(tl);
    targetoutputs := second(tl);
    if (*delta-rule-debug-flag*)
      print(list("Current targets:", targetoutputs), *standard-output*);
    end if;
    idimension := head(sizelist);
    //  get the size of the input slab
    iactivationvector := head(activationlist);
    //  get array of input activations
    for (i from 0 below idimension)
      //  copy training inputs to input slab
      iactivationvector[i] := inputs[i] + frandom(- inputnoise, inputnoise);
    end for;
    // 
    //  Propagate activation through all of the slabs:
    //
    for (n-1 from 0 below nlayers - 1)
      //  update layer i to layer flowing to layer j
      n := n-1 + 1;
      jdimension := sizelist[n];
      //  get the size of the j'th layer
      jactivationvector := activationlist[n];
      //  activation array for slab j
      weightarray := weightlist[n-1];
      sumofproductsarray := sumofproductslist[n-1];
      for (j from 0 below jdimension)
        //  process each neuron in slab j
        sum := 0.0;
        //  init sum of products to zero
        for (i from 0 below idimension)
          //  to get activation from each neuron in previous slab
          sum := sum + weightarray[i, j] * iactivationvector[i];
        end for;
        sumofproductsarray[j] := sum;
        //  save sum of products
        jactivationvector[j] := sigmoid(sum);
      end for;
      idimension := jdimension;
      //  reset index for next slab pair
      iactivationvector := jactivationvector;
    end for;
    // 
    //  Activation is spread through the network and sum of products calculated.
    //  Now modify the weights in the network using back error propagation. Start
    //  by calculating the error signal for each neuron in the output layer:
    //
    jdimension := sizelist[nlayers - 1];
    //  size of last layer
    jactivationvector := activationlist[nlayers - 1];
    jdeltavector := deltalist[nlayers - 2];
    sumofproductsarray := sumofproductslist[nlayers - 2];
    outputerror := 0;
    for (j from 0 below jdimension)
      delta := targetoutputs[j] - jactivationvector[j];
      outputerror := outputerror + abs(delta);
      jdeltavector[j]
                    := jdeltavector[j]
                                     + delta
                                        * dsigmoid(sumofproductsarray[j]);
    end for;
    // 
    //  Now calculate the backpropagated error signal for all hidden slabs:
    //
    for (nn from 0 below nlayers - 2)
      n := nlayers - 3 - nn;
      idimension := sizelist[n + 1];
      isumofproductsarray := sumofproductslist[n];
      ideltavector := deltalist[n];
      for (i from 0 below idimension) ideltavector[i] := 0.0; end for;
      begin weightarray := n[\+]; weightlist := #f; end;
    end for;
    for (i from 0 below idimension)
      error := 0.0;
      for (i from 0 below idimension)
        error := error + jdeltavector[j] * weightarray[i, j];
      end for;
      ideltavector[i]
                    := ideltavector[i]
                                     + error
                                        * dsigmoid(isumofproductsarray[i]);
    end for;
    jdimension := idimension;
    jdeltavector := ideltavector;
  end for;
  // 
  //  Update all delta weights in the network:
  //
  idimension := head(sizelist);
  for (n from 0 below nlayers - 1)
    iactivationvector := activationlist[n];
    jdimension := sizelist[n + 1];
    jdeltavector := deltalist[n];
    deltaweightarray := deltaweightlist[n];
    weightarray := weightlist[n];
    eida := eidalist[n];
    for (j from 0 below jdimension)
      for (i from 0 below idimension)
        delta := eida * jdeltavector[j] * iactivationvector[i];
        deltaweightarray[i, j] := deltaweightarray[i, j] + delta;
      end for;
    end for;
    //  remember delta weight change
    idimension := jdimension;
  end for;
  if (~ (autoplot = #"no")) deltaplot(netlist, input-format); end if;
  // 
  //  Update all weights in the network:
  //
  idimension := head(sizelist);
  for (n from 0 below nlayers - 1)
    iactivationvector := activationlist[n];
    jdimension := sizelist[n + 1];
    jdeltavector := deltalist[n];
    deltaweightarray := deltaweightlist[n];
    olddeltaweightarray := olddeltaweightlist[n];
    weightarray := weightlist[n];
    for (j from 0 below jdimension)
      for (i from 0 below idimension)
        weiqhtarray[i,
                    j]
                     := weightarray[i, j] + alpha * deltaweightarray[i, j]
                         + beta * olddeltaweightarray[i, j];
        olddeltaweightarray[i,
                            j]
                             := //  save current delta weights
                            deltaweightarray[i,
                                             j];
      end for;
    end for;
    //  ...for next momentum term.
    idimension := jdimension;
  end for;
  outputerror / jdimension;
end method deltalearn;

// 
//  Utility for using a trained neural network in the recall mode.
//  The first argument to this function is a network definition (as
//  returned from NewDeltaNetwork) and the second argument is a list
//  of input neuron activation values to drive through the network.
//
define method deltarecall (netlist, inputs)
  let nlayers = head(netlist);
  let sizelist = second(netlist);
  let activationlist = third(netlist);
  let weightlist = second(cdddr(netlist));
  let idimension = #f;
  let jdimension = #f;
  let iactivationvector = #f;
  let jactivationvector = #f;
  let n = #f;
  let weightarray = #f;
  let returnlist = #f;
  let sum = #f;
  idimension := head(sizelist);
  // get the size of ths input slab
  iactivationvector := head(activationlist);
  // get array of input activations
  for (i from 0 below idimension)
    //  copy training inputs to input slab
    iactivationvector[i] := inputs[i];
  end for;
  for (n-1 from 0 below nlayers - 1)
    n := n-1 + 1;
    jdimension := sizelist[n];
    //  get the size of the j'th layer
    jactivationvector := activationlist[n];
    //  activation array for slab j
    weightarray := weightlist[n-1];
    for (j from 0 below jdimension)
      //  process sach neuron in slab j
      sum := 0.0;
      //  init sum of products to zero
      for (i from 0 below idimension)
        //  to get activation from each neuron in previous slab
        sum := sum + weightarray[i, j] * iactivationvector[i];
      end for;
      if (*delta-rule-debug-flag*)
        print(list("sum =", sum), *standard-output*);
      end if;
      jactivationvector[j] := sigmoid(sum);
    end for;
    idimension := jdimension;
    //  get ready for next slab pair
    iactivationvector := jactivationvector;
  end for;
  for (i from 0 below jdimension)
    returnlist := concatenate(returnlist, list(jactivationvector[j]));
  end for;
  returnlist;
end method deltarecall;

// 
//  Utilities to plot a network
//
define method plotactivations (title, x, y, data, dmin, dmax,
                               #key input-format = #f)
  let size = dimension(data, 0);
  let ypos = 0;
  let xpos = x;
  plot-string(x, y - 10, title);
  if (input-format)
    let x-size = head(input-format);
    let y-size = second(input-format);
    for (yy from 0 below y-size)
      for (xx from 0 below x-size)
        plot-size-rect(truncate(xpos + 9 * xx), truncate(ypos + 9 * yy + 20),
                       8, 8,
                       truncate((data[yy * x-size + xx] - dmin) / (dmax - dmin)
                                 * 8));
      end for;
    end for;
  else
    for (i from 0 below size)
      if (size < 20)
        ypos := y;
        xpos := x + i * 9;
      elseif (i < size / 2)
        begin ypos := y - 7; xpos := x + i * 9; end;
      else
        ypos := y + 2;
        xpos := x + (i - size / 2) * 9;
      end if;
      plot-size-rect(truncate(xpos), truncate(ypos), 8, 8,
                     truncate((data[i] - dmin) / (dmax - dmin) * 8));
      plot-frame-rect(truncate(xpos), truncate(ypos), 8, 8);
    end for;
  end if;
end method plotactivations;

define method plotweights (title, x, y, data, dmin, dmax, deltaweights)
  let xsize = dimension(data, 0);
  let ysize = dimension(data, 1);
  if (xsize * ysize < 200)
    plot-string(x + 20, y - 10, title);
    for (i from 0 below xsize)
      for (j from 0 below ysize)
        plot-size-rect(x + i * 9, y + j * 9, 8, 8,
                       truncate((data[i, j] - dmin) / (dmax - dmin) * 8));
        plot-frame-rect(x + i * 9, y + j * 9, 8, 8);
        plot-size-rect(xsize * 8 + 30 + x + i * 9, y + j * 9, 8, 8,
                       truncate((deltaweights[i, j] - -0.05) / (0.05 - -0.05)
                                 * 8));
        plot-frame-rect(xsize * 8 + 30 + x + i * 9, y + j * 9, 8, 8);
      end for;
    end for;
  end if;
end method plotweights;

define method deltaplot (netlist, input-format,
                         #key plotonlyifmouseclick = #"yes")
  if (~ (plotonlyifmouseclick = #"yes") | plot-mouse-down())
    let nlayers = head(netlist);
    let sizelist = second(netlist);
    let activationlist = third(netlist);
    let sumofproductslist = head(cdddr(netlist));
    let weightlist = second(cdddr(netlist));
    let deltaweightlist = third(cdddr(netlist));
    let minscale = -0.3;
    let maxscale = 0.3;
    let y-start = 0;
    show-plot();
    plot-string-bold(20, 15, "Delta Network");
    plot-string(20, 30, "Activation slabs:");
    plotactivations("slab1", 10, 60, activationlist[0], -0.5, 0.5,
                                                    input-format);
    if (input-format)
      y-start := 30 + second(input-format) * 11;
    else
      y-start := 60;
    end if;
    for (n-1 from 0 below nlayers - 1)
      n := n-1 + 1;
      if (n = nlayers - 1) minscale := -0.1; maxscale := 0.1; end if;
      //  scale up output display
      plotactivations(#("slab1", "slab2", "slab3", "slab4", "slab5")[n],
                                                                     //  title
                                                                     10,
                                                                     //  x location for subplot
                                                                     y-start
                                                                      + n
                                                                         * 35,
                                                                     //  Y location for subplot
                                                                     activationlist[n],
                                                                                    //  data to plot as gray scale
                                                                                    minscale,
                                                                                    maxscale,
                                                                                    #f);
    end for;
    if (nlayers < 4)
      y-start := y-start + 100;
      plot-string(20, y-start, "Weights and Delta Weights:");
      y-start := y-start + 20;
      for (n from 0 below nlayers - 1)
        plotweights(#("slabl -> slab2", "slab2 -> slab3",
                      "slab3 -> slab4")[n],
                                        10, y-start + n * 70,
                                        //  x,y position of subplot
                                        weightlist[n], -1.0, 1.0,
                                                   deltaweightlist[n]);
      end for;
    end if;
  end if;
end method deltaplot;

// 
//  Calculate Sigmoid and derivative of Sigmoid functions:
//
define method sigmoid (x) 1.0 / (1.0 + exp(- x)); end method sigmoid;

define method dsigmoid (x)
  let temp = sigmoid(x);
  temp * (1.0 - temp);
end method dsigmoid;

// 
//  Generate floating point random numbers:
//
define method frandom (low, high)
  let range = high - low;
  random-uniform(to: 1000) / 1000.0 * range + low;
end method frandom;

// 
//  Save a Delta Network to a disk file:
//
define method writedeltanetwork (filename, netlist)
  let filestream
      = make(<file-stream>, locator: filename, direction: #"output");
  let nlayers = head(netlist);
  let sizelist = second(netlist);
  let activationlist = third(netlist);
  let weightlist = second(cdddr(netlist));
  let deltaweightlist = third(cdddr(netlist));
  let olddeltaweightlist = head(cddddr(cdddr(netllst)));
  let alpha = second(cddddr(cdddr(netlist)));
  let beta = third(cddddr(cdddr(netljst)));
  // 
  //  Write out header
  //
  print(nlayers, filestream);
  print(sizelist, filestream);
  // 
  //  Write out activations:
  //
  for (n from 0 below nlayers)
    for (i from 0 below sizelist[n])
      print(activationlist[n][i], filestream);
    end for;
  end for;
  // 
  // Write out weights:
  //
  for (n from 0 below nlayers - 1)
    let w = weightlist[n];
    for (i from 0 below dimension(w, 0))
      for (j from 0 below dimension(w, 1))
        print(w[i, j], filestream);
      end for;
    end for;
  end for;
  // 
  //  Write out delta weights:
  //
  for (n from 0 below nlayers - 1)
    let w = deltaweightlist[n];
    for (i from 0 below dimension(w, 0))
      for (j from 0 below dimension(w, 1))
        print(w[i, j], filestream);
      end for;
    end for;
  end for;
  // 
  //  Write out old delta weights:
  //
  for (n from 0 below nlayers - 1)
    let w = olddeltaweightlist[n];
    for (i from 0 below dimension(w, 0))
      for (j from 0 below dimension(w, 1))
        print(w[i, j], filestream);
      end for;
    end for;
  end for;
  // 
  //  Write alpha, beta terms (used for momentum):
  //
  print(alpha, filestream);
  print(beta, filestream);
  close(filestream);
end method writedeltanetwork;

// 
//  Read a delta detwork from a disk file:
//
define method readdeltanetwork (filename)
  let filestream
      = make(<file-stream>, locator: filename, direction: #"input");
  let numlayers = #f;
  let sizelist = #f;
  let a-list = #f;
  let s-list = #f;
  let w-list = #f;
  let dw-list = #f;
  let old-dw-list = #f;
  let d-list = #f;
  let alpha = #f;
  let beta = #f;
  // 
  //  Read in header:
  //
  numlayers
   := // LTD: Function READ not yet implemented.
      read(filestream);
  sizelist
   := // LTD: Function READ not yet implemented.
      read(filestream);
  // 
  //  Allocate array storage:
  //
  a-list
   := map(method (size) make(<array>, dimensions: list(size)); end method,
          sizelist);
  s-list
   := map(method (size) make(<array>, dimensions: list(size)); end method,
          tail(sizelist));
  for (i from 0 below numlayers - 1)
    w-list := pair(list(sizelist[i], sizelist[i + 1]), w-list);
  end for;
  w-list
   := map(method (size) make(<array>, dimensions: size); end method,
          reverse(w-list));
  for (i from 0 below numlayers - 1)
    dw-list := pair(list(sizelist[i], sizelist[i + 1]), dw-list);
  end for;
  for (i from 0 below numlayers - 1)
    old-dw-list := pair(list(sizelist[i], sizelist[i + 1]), old-dw-list);
  end for;
  dw-list
   := map(method (size) make(<array>, dimensions: size); end method,
          reverse(dw-list));
  old-dw-list
   := map(method (size) make(<array>, dimensions: size); end method,
          reverse(old-dw-list));
  // 
  //  Read in activations:
  //
  for (n from 0 below numlayers)
    for (i from 0 below sizelist[n])
      a-list[n][i]
                 := // LTD: Function READ not yet implemented.
                    read(filestream);
    end for;
  end for;
  // 
  //  Read in weights:
  //
  for (n from 0 below numlayers - 1)
    let w = w-list[n];
    for (i from 0 below dimension(w, 0))
      for (j from 0 below dimension(w, 1))
        w[i,
          j]
           := // LTD: Function READ not yet implemented.
              read(filestream);
      end for;
    end for;
  end for;
  // 
  //  Read in delta weights:
  //
  for (n from 0 below numlayers - 1)
    let w = dw-list[n];
    for (i from 0 below dimension(w, 0))
      for (j from 0 below dimension(w1))
        w[i,
          j]
           := // LTD: Function READ not yet implemented.
              read(filestream);
      end for;
    end for;
  end for;
  // 
  //  Read in old delta weights:
  //
  for (n from 0 below numlayers - 1)
    let w = old-dw-list[n];
    for (i from 0 below dimension(w, 0))
      for (j from 0 below dimension(w, 1))
        w[i,
          j]
           := // LTD: Function READ not yet implemented.
              read(filestream);
      end for;
    end for;
  end for;
  begin
    alpha
     := // LTD: Function READ not yet implemented.
        read(filestream);
    beta
     := // LTD: Function READ not yet implemented.
        read(filestream);
  end;
  close(filestream);
  list(numlayers, sizelist, a-list, s-list, w-list, dw-list, d-list,
       old-dw-list, alpha, beta);
end method readdeltanetwork;

// 
//  Throw away test functions for two, three and four layer networks:
//
#f;

//  temp will be used to hold the network data
define method test2 (#key restart = #"yes")
  if (restart = #"yes") temp := newdeltanetwork(#(2, 2)); end if;
  //  specify a two layer network
  for (ii from 0 below 10000)
    rmserror
     := deltalearn(temp, #(#(#(1, 0), #(0, 1)), #(#(0, 1), #(1, 0))), #"yes");
    //  autoplot mode
    if (modulo(ii, 50) = 0)
      print("....training cycle #", *standard-output*);
      print(ii, *standard-output*);
      print(" RMS error = ", *standard-output*);
      print(rmserror, *standard-output*);
      write-element(*standard-output*, '\n');
    end if;
  end for;
end method test2;

define method test3 (#key restart = #"yes")
  //  three layer network
  if (restart = #"yes") temp := newdeltanetwork(#(5, 4, 5)); end if;
  for (ii from 0 below 10000)
    rmserror
     := deltalearn(temp,
                   #(#(#(1, 0, 0, 0, 0), #(0, 1, 0, 0, 0)),
                     #(#(0, 1, 0, 0, 0), #(0, 0, 1, 0, 0)),
                     #(#(0, 0, 1, 0, 0), #(0, 0, 0, 1, 0)),
                     #(#(0, 0, 0, 1, 0), #(0, 0, 0, 0, 1)),
                     #(#(0, 0, 0, 0, 1), #(1, 0, 0, 0, 0))),
                   #"yes");
    //  autoplot mode
    if (modulo(ii, 50) = 0)
      print("....training cycle #", *standard-output*);
      print(ii, *standard-output*);
      print(" RMS error = ", *standard-output*);
      print(rmserror, *standard-output*);
      write-element(*standard-output*, '\n');
    end if;
  end for;
end method test3;

define method test4 (#key restart = #"yes")
  //  four layer network
  if (restart = #"yes") temp := newdeltanetwork(#(4, 5, 5, 4)); end if;
  for (ii from 0 below 10000)
    rmserror
     := deltalearn(temp,
                   #(#(#(1, 0, 0, 0), #(0, 1, 0, 0)),
                     #(#(0, 1, 0, 0), #(0, 0, 1, 0)),
                     #(#(0, 0, 1, 0), #(0, 0, 0, 1)),
                     #(#(0, 0, 0, 1), #(1, 0, 0, 0))),
                   #"yes");
    //  autoplot mode
    if (modulo(ii, 50) = 0)
      print("....training cycle #", *standard-output*);
      print(ii, *standard-output*);
      print(" RMS error = ", *standard-output*);
      print(rmserror, *standard-output*);
      write-element(*standard-output*, '\n');
    end if;
  end for;
end method test4;

