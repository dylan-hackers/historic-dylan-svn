// ----------------------------------------------------------------------------
// Artificial Intelligence, Second Edition
// Elaine Rich and Kevin Knight
// McGraw Hill, 1991
// 
// This code may be freely copied and used for educational or research purposes.
// All software written by Kevin Knight.
// Comments, bugs, improvements to knight@cs.cmu.edu
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// 			BACKPROPAGATION ALGORITHM
// 			 (SINGLE, BINARY OUTPUT)
// 			     "backprop.lisp"
// ----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// 
//    Backpropagation algorithm (single, binary output) 
//  
//    To use this program, you must set up three input files: a ".txt",
//    a ".train", and a ".test".  Here is an example:
// 
//    File xor.txt:
// 
// 	Network structure:			(2 2 1)
// 	Epochs:					801
// 	Test after every N epochs:		100
// 	Learning rate (eta):			0.35
// 	Momentum (alpha):			0.90
// 	Noise?:					0
// 	Training data:				xor.train
// 	Testing data:			        xor.test
// 
//    File xor.train:
// 
// 	0 0 0
// 	0 1 1
// 	1 0 1
// 	1 1 0
// 
//    File xor.test:
// 
// 	0 0 0
// 	1 0 1
// 
//   The main file contains settings for various parameters.  There are 
//   separate files for training and testing data.  
// 
//   To run, call (backprop "xor.txt") (or whatever the filename is).
//   The program will periodically append its results to the end of the 
//   "xor.txt" file.  This version of backpropagation is geared toward 
//   networks with a single, binary output.  It provides periodic analyses
//   of how well the network is predicting the output bit as learning 
//   progresses.
// 
// ----------------------------------------------------------------------------
// 
//  Variables.
//
define variable network = #f;

define variable output-layer = #f;

define variable structure = #f;

define variable total-epochs = 0;

define variable test-interval = 0;

define variable training-inputs = #f;

define variable training-outputs = #f;

define variable testing-inputs = #f;

define variable testing-outputs = #f;

define variable total-training = #f;

define variable total-testing = #f;

define variable total-inputs = #f;

define variable total-outputs = #f;

define variable eta = 0;

define variable alpha = 0;

define variable noise = 0;

define variable train-file = #f;

define variable test-file = #f;

define variable total-guessed = make(<vector>, size: 8);

define variable total-right = make(<vector>, size: 8);

// 
//  Structures.
//
define class <unit> (<object>)
  slot unit-weighted-sum = #"float", init-keyword: #"unit-weighted-sum";
  slot unit-activation = #"float", init-keyword: #"unit-activation";
  slot unit-delta = #"float", init-keyword: #"unit-delta";
end class <unit>;

define class <net> (<object>)
  slot net-units, init-keyword: #"net-units";
  slot net-connections, init-keyword: #"net-connections";
  slot net-size, init-keyword: #"net-size";
  slot net-next-layer, init-keyword: #"net-next-layer";
  slot net-prev-layer, init-keyword: #"net-prev-layer";
end class <net>;

define class <connection> (<object>)
  slot connection-weight = #"float", init-keyword: #"connection-weight";
  slot connection-delta-weight = #"float",
       init-keyword: #"connection-delta-weight";
end class <connection>;

define method output-layer? (layer)
  empty?(layer.net-next-layer);
end method output-layer?;

define method input-layer? (layer)
  empty?(layer.net-prev-layer);
end method input-layer?;

define method hidden-layer? (layer)
  layer.net-next-layer & layer.net-prev-layer;
end method hidden-layer?;

// 
//  Building the network.
//
define method random-real (lo, hi)
  lo + random-uniform(to: (hi - lo));
end method random-real;

define method random-weight () random-real(-0.8, 0.8); end method random-weight;

define method random-noise () random-real(0.0, 0.15); end method random-noise;

define method construct-units ()
  for (n = structure then cdr(n), last-layer = nil then nil,
       temp-net = nil then nil, until empty?(n))
    temp-net
     := make-net(units: make(<array>, dimensions: head(n) + 1),
                 connections: if (tail(n))
                                make(<array>,
                                     dimensions: list(head(n) + 1,
                                                      second(n) + 1));
                              else
                                #f;
                              end if,
                 prev-layer: last-layer);
    for (u = 0 then 1+(u), until u > head(n))
      temp-net.net-units[u] := make-unit();
    finally
      #f;
    end for;
    if (tail(n))
      for (u1 = 0 then 1+(u1), until u1 > head(n))
        for (u2 = 1 then 1+(u2), until u2 > second(n))
          temp-net.net-connections[u1, u2] := make-connection();
        finally
          #f;
        end for;
      finally
        #f;
      end for;
    end if;
    if (size(n) = size(structure))
      network := temp-net;
      temp-net.net-size := head(n);
      last-layer := temp-net;
    else
      last-layer.net-next-layer := temp-net;
      temp-net.net-size := head(n);
      last-layer := temp-net;
    end if;
  finally
    output-layer := last-layer;
  end for;
end method construct-units;

define method set-initial-weights ()
  for (layer = network then layer.net-next-layer, until empty?(layer))
    let size = layer.net-size;
    let units = layer.net-units;
    let connections = layer.net-connections;
    for (u = 0 then 1+(u), until u > size)
      unit-activation(units[u]) := 0;
      unit-weighted-sum(units[u]) := 0;
      unit-delta(units[u]) := 0;
    finally
      #f;
    end for;
    if (~ output-layer?(layer))
      let next-layer-size = layer.net-next-layer.net-size;
      for (u1 = 0 then 1+(u1), until u1 > size)
        for (u2 = 1 then 1+(u2), until u2 > next-layer-size)
          connection-weight(connections[u1, u2]) := random-weight();
          connection-delta-weight(connections[u1, u2]) := 0.0;
        finally
          #f;
        end for;
      finally
        #f;
      end for;
    end if;
  finally
    #f;
  end for;
  for (layer = network then layer.net-next-layer, until empty?(layer))
    unit-activation(layer.net-units[0]) := 1.0;
  finally
    #f;
  end for;
end method set-initial-weights;

define method build-network ()
  total-inputs := head(structure);
  total-outputs := head(copy-sequence(structure, start: size(structure) - 1));
  construct-units();
  set-initial-weights();
end method build-network;

// 
//  Building the test and train sets.
//
define method build-test-and-train-sets ()
  total-training := 0;
  with-open-file (ifile = (train-file, direction: #"input"))
    for (x = read-line(ifile, nil, #"error") then read-line(ifile,
                                                            nil,
                                                            #"error"),
         tot = 0 then 0, until string-equal?("", x) | x == #"error")
      if (~ string-equal?("", x)) tot := tot + 1; end if;
    finally
      total-training := tot;
    end for;
  end with-open-file;
  total-testing := 0;
  with-open-file (ifile = (test-file, direction: #"input"))
    for (x = read-line(ifile, nil, #"error") then read-line(ifile,
                                                            nil,
                                                            #"error"),
         tot = 0 then 0, until string-equal?("", x) | x == #"error")
      if (~ string-equal?("", x)) tot := tot + 1; end if;
    finally
      total-testing := tot;
    end for;
  end with-open-file;
  training-inputs
   := make(<array>, dimensions: list(total-training + 1, total-inputs + 1));
  training-outputs
   := make(<array>, dimensions: list(total-training + 1, total-outputs + 1));
  testing-inputs
   := make(<array>, dimensions: list(total-testing + 1, total-inputs + 1));
  testing-outputs
   := make(<array>, dimensions: list(total-testing + 1, total-outputs + 1));
  with-open-file (ifile = (train-file, direction: #"input"))
    for (x = 1 then 1+(x), until x > total-training)
      for (y = 1 then 1+(y), until y > total-inputs)
        training-inputs[x,
                        y]
                         := // LTD: Function READ not yet implemented.
                            read(ifile);
      finally
        #f;
      end for;
      for (y = 1 then 1+(y), until y > total-outputs)
        training-outputs[x,
                         y]
                          := bound-outputs(// LTD: Function READ not yet implemented.
                                           read(ifile));
      finally
        #f;
      end for;
    finally
      #f;
    end for;
  end with-open-file;
  with-open-file (ifile = (test-file, direction: #"input"))
    for (x = 1 then 1+(x), until x > total-testing)
      for (y = 1 then 1+(y), until y > total-inputs)
        testing-inputs[x,
                       y]
                        := // LTD: Function READ not yet implemented.
                           read(ifile);
      finally
        #f;
      end for;
      for (y = 1 then 1+(y), until y > total-outputs)
        testing-outputs[x,
                        y]
                         := bound-outputs(// LTD: Function READ not yet implemented.
                                          read(ifile));
      finally
        #f;
      end for;
    finally
      #f;
    end for;
  end with-open-file;
end method build-test-and-train-sets;

//  Function BOUND-OUTPUTS takes a target output from a testing or training 
//  file, and forces it to be at least 0.1 and at most 0.9.
define method bound-outputs (x)
  if (x > 0.9) 0.9; elseif (x < 0.1) 0.1; else x; end if;
end method bound-outputs;

define method init-network (verbose)
  if (verbose) format-out("Building network ...\n"); end if;
  build-network();
  if (verbose) format-out("Building test and train sets ...\n"); end if;
  build-test-and-train-sets();
end method init-network;

// 
//  Reading the input file.
//
define method read-data (infile, verbose)
  if (verbose) format-out("Reading input data ...\n"); end if;
  with-open-file (ifile = (infile, direction: #"input"))
    for (x = read-char(ifile) then read-char(ifile), until x = ':')
      #f;
    finally
      #f;
    end for;
    structure
     := // LTD: Function READ not yet implemented.
        read(ifile);
    for (x = read-char(ifile) then read-char(ifile), until x = ':')
      #f;
    finally
      #f;
    end for;
    total-epochs
     := // LTD: Function READ not yet implemented.
        read(ifile);
    for (x = read-char(ifile) then read-char(ifile), until x = ':')
      #f;
    finally
      #f;
    end for;
    test-interval
     := // LTD: Function READ not yet implemented.
        read(ifile);
    for (x = read-char(ifile) then read-char(ifile), until x = ':')
      #f;
    finally
      #f;
    end for;
    eta
     := // LTD: Function READ not yet implemented.
        read(ifile);
    for (x = read-char(ifile) then read-char(ifile), until x = ':')
      #f;
    finally
      #f;
    end for;
    alpha
     := // LTD: Function READ not yet implemented.
        read(ifile);
    for (x = read-char(ifile) then read-char(ifile), until x = ':')
      #f;
    finally
      #f;
    end for;
    noise
     := // LTD: Function READ not yet implemented.
        read(ifile);
    for (x = read-char(ifile) then read-char(ifile), until x = ':')
      #f;
    finally
      #f;
    end for;
    train-file := remove!(remove!(read-line(ifile, nil), ' '), '\t');
    for (x = read-char(ifile) then read-char(ifile), until x = ':')
      #f;
    finally
      #f;
    end for;
    test-file := remove!(remove!(read-line(ifile, nil), ' '), '\t');
  end with-open-file;
end method read-data;

// 
//  Activation function.
//
define variable zero-float = as(<float>, 0.0);

define method activation-function (sum)
  1.0 / (1.0 + exp((zero-float - sum)));
end method activation-function;

// 
//  Multiplication.
//
define method mult (#rest args)
  //   (without-floating-underflow-traps (apply #'* args)))
  apply(\*, args);
end method mult;

// 
//  Feed Forward phase.
//
define method feed-forward (index, set, verbose)
  if (verbose)
    format-out("Feeding vector %d from %d into network ...\n", index,
               if (set == training-inputs) 0; else 1; end if);
  end if;
  for (u = 1 then 1+(u), until u > network.net-size)
    let noise-added = if (noise = 1) random-noise(); else 0.0; end if;
    let this-input = set[index, u];
    let input-plus-noise
        = if (this-input > 0.5)
            this-input - noise-added;
          else
            this-input + noise-added;
          end if;
    unit-activation(network.net-units[u]) := input-plus-noise;
  finally
    #f;
  end for;
  for (layer = network then layer.net-next-layer, until output-layer?(layer))
    for (u1 = 1 then 1+(u1), until u1 > layer.net-next-layer.net-size)
      let this-unit = layer.net-next-layer.net-units[u1];
      this-unit.unit-weighted-sum := 0.0;
      for (u2 = 0 then 1+(u2), until u2 > layer.net-size)
        this-unit.unit-weighted-sum
         := this-unit.unit-weighted-sum
             + mult(unit-activation(layer.net-units[u2]),
                    connection-weight(layer.net-connections[u2, u1]));
      finally
        #f;
      end for;
      this-unit.unit-activation
       := activation-function(this-unit.unit-weighted-sum);
    finally
      #f;
    end for;
  finally
    #f;
  end for;
  if (verbose)
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 write-string++("Result = ", xp, 0, 9);
                 using-format(xp, "~14,7f", pop!(args));
                 write-string++(" ...", xp, 0, 4);
                 pprint-newline+(unconditional: xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#t, unit-activation(output-layer.net-units[1]));
  end if;
end method feed-forward;

// 
//  Back Propagate phase.
//
define method back-propagate (index, temp-alpha, verbose)
  if (verbose) format-out("Backpropagating errors ...\n"); end if;
  for (u = 1 then 1+(u), until u > output-layer.net-size)
    let this-unit = output-layer.net-units[u];
    this-unit.unit-delta
     := mult(training-outputs[index, u] - this-unit.unit-activation,
             this-unit.unit-activation, 1.0 - this-unit.unit-activation);
  finally
    #f;
  end for;
  for (layer = output-layer.net-prev-layer then layer.net-prev-layer,
       until input-layer?(layer))
    for (u = 0 then 1+(u), until u > layer.net-size)
      let this-unit = layer.net-units[u];
      let sum = 0.0;
      for (u2 = 1 then 1+(u2), until u2 > layer.net-next-layer.net-size)
        sum
         := sum
             + mult(unit-delta(layer.net-next-layer.net-units[u2]),
                    connection-weight(layer.net-connections[u, u2]));
      end for;
      this-unit.unit-delta
       := mult(1.0 - this-unit.unit-activation, this-unit.unit-activation,
               sum);
    end for;
  finally
    #f;
  end for;
  for (layer = output-layer.net-prev-layer then layer.net-prev-layer,
       l = 1 then 1+(l), until empty?(layer))
    for (u = 0 then 1+(u), until u > layer.net-size)
      let low-unit = layer.net-units[u];
      for (u2 = 1 then 1+(u2), until u2 > layer.net-next-layer.net-size)
        let hi-unit = layer.net-next-layer.net-units[u2];
        let the-connection = layer.net-connections[u, u2];
        let newchange
            = mult(eta, hi-unit.unit-delta, low-unit.unit-activation)
               + mult(temp-alpha, the-connection.connection-delta-weight);
        if (verbose)
          (method (s, #rest args)
             apply(maybe-initiate-xp-printing,
                   method (xp, #rest args)
                     begin
                       write-string++("Changing weight (", xp, 0, 17);
                       using-format(xp, "~d", pop!(args));
                       write-char++(' ', xp);
                       using-format(xp, "~d", pop!(args));
                       write-char++(' ', xp);
                       using-format(xp, "~d", pop!(args));
                       write-string++(") from ", xp, 0, 7);
                       using-format(xp, "~14,7f", pop!(args));
                       write-string++(" to ", xp, 0, 4);
                       using-format(xp, "~14,7f", pop!(args));
                       pprint-newline+(unconditional: xp);
                     end;
                     if (args) copy-sequence(args); end if;
                   end method,
                   s, args);
           end method)(#t, l, u, u2, the-connection.connection-weight,
                       the-connection.connection-weight + newchange);
        end if;
        the-connection.connection-weight
         := the-connection.connection-weight + newchange;
        the-connection.connection-delta-weight := newchange;
      finally
        #f;
      end for;
    finally
      #f;
    end for;
  finally
    #f;
  end for;
end method back-propagate;

// 
//  Learn.
//
define method learn (infile, verbose)
  for (epoch from 0 below total-epochs)
    if (verbose) format-out("Starting epoch %d ...\n", epoch); end if;
    let temp-alpha = if (epoch < 10) 0.0; else alpha; end if;
    if (0 = modulo(epoch, test-interval))
      evaluate-progress(epoch, infile);
    end if;
    for (x = 1 then 1+(x), until x > total-training)
      feed-forward(x, training-inputs, verbose);
      back-propagate(x, temp-alpha, verbose);
    end for;
  end for;
end method learn;

// 
//  Evaluate Progress.
//
define method evaluate-progress (epoch, infile)
  evaluate-training-data(epoch, infile);
  evaluate-testing-data(epoch, infile);
end method evaluate-progress;

define method evaluate-training-data (epoch, infile)
  for (i = 0 then 1+(i), until i = 8) total-guessed[i] := 0.0; end for;
  for (i = 0 then 1+(i), until i = 8) total-right[i] := 0.0; end for;
  for (i = 1 then 1+(i), until i > total-training)
    let right = training-outputs[i, 1];
    feed-forward(i, training-inputs, #f);
    let output-activation = unit-activation(output-layer.net-units[1]);
    for (j = 0 then 1+(j), until j = 8)
      if (output-activation > 0.5 + 0.05 * j
           | output-activation < 0.5 - 0.05 * j)
        total-guessed[j] := 1.0 + total-guessed[j];
        if (right > 0.5 & output-activation > 0.5 + 0.05 * j
             | (right < 0.5 & output-activation < 0.5 - 0.05 * j))
          total-right[j] := 1.0 + total-right[j];
        end if;
      end if;
    end for;
  end for;
  with-open-file (ifile
                   = (infile, direction: #"output", if-exists: #"append"))
    format(ifile, "EPOCH %d.  Performance on training data:\n\n", epoch);
    format(ifile, "Confidence: ");
    for (i = 0 then 1+(i), until i = 8)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~6,2f", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(ifile, i * 0.05);
    end for;
    format(ifile, "\n");
    format(ifile, "Guessed:    ");
    for (i = 0 then 1+(i), until i = 8)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~6d", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(ifile, round(total-guessed[i]));
    end for;
    format(ifile, "\n");
    format(ifile, "Correct:    ");
    for (i = 0 then 1+(i), until i = 8)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~6d", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(ifile, round(total-right[i]));
    end for;
    format(ifile, "\n");
    format(ifile, "Percent:    ");
    for (i = 0 then 1+(i), until i = 8)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~6,2f", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(ifile,
                   if (total-guessed[i] > 0.5)
                     100.0 * total-right[i] / total-guessed[i];
                   else
                     0.0;
                   end if);
    end for;
    format(ifile, "\n\n");
  end with-open-file;
end method evaluate-training-data;

define method evaluate-testing-data (epoch, infile)
  for (i = 0 then 1+(i), until i = 8) total-guessed[i] := 0.0; end for;
  for (i = 0 then 1+(i), until i = 8) total-right[i] := 0.0; end for;
  for (i = 1 then 1+(i), until i > total-testing)
    let right = testing-outputs[i, 1];
    feed-forward(i, testing-inputs, #f);
    let output-activation = unit-activation(output-layer.net-units[1]);
    for (j = 0 then 1+(j), until j = 8)
      if (output-activation > 0.5 + 0.05 * j
           | output-activation < 0.5 - 0.05 * j)
        total-guessed[j] := 1.0 + total-guessed[j];
        if (right > 0.5 & output-activation > 0.5 + 0.05 * j
             | (right < 0.5 & output-activation < 0.5 - 0.05 * j))
          total-right[j] := 1.0 + total-right[j];
        end if;
      end if;
    end for;
  end for;
  with-open-file (ifile
                   = (infile, direction: #"output", if-exists: #"append"))
    format(ifile, "EPOCH %d.  Performance on testing data:\n\n", epoch);
    format(ifile, "Confidence: ");
    for (i = 0 then 1+(i), until i = 8)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~6,2f", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(ifile, i * 0.05);
    end for;
    format(ifile, "\n");
    format(ifile, "Guessed:    ");
    for (i = 0 then 1+(i), until i = 8)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~6d", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(ifile, round(total-guessed[i]));
    end for;
    format(ifile, "\n");
    format(ifile, "Correct:    ");
    for (i = 0 then 1+(i), until i = 8)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~6d", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(ifile, round(total-right[i]));
    end for;
    format(ifile, "\n");
    format(ifile, "Percent:    ");
    for (i = 0 then 1+(i), until i = 8)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~6,2f", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(ifile,
                   if (total-guessed[i] > 0.5)
                     100.0 * total-right[i] / total-guessed[i];
                   else
                     0.0;
                   end if);
    end for;
    format(ifile, "\n\n");
  end with-open-file;
end method evaluate-testing-data;

// 
//  Print Weights.
//
define method print-weights (infile)
  with-open-file (ifile
                   = (infile, direction: #"output", if-exists: #"append"))
    format(ifile, "\n\nWeights:\n\n");
    for (layer = network then layer.net-next-layer, k = 1 then 1+(k),
         until output-layer?(layer))
      for (u = 0 then 1+(u), until u > layer.net-size)
        for (u2 = 1 then 1+(u2), until u2 > layer.net-next-layer.net-size)
          let the-connection = layer.net-connections[u, u2];
          format(ifile, "(~d ~d ~d ~14.7f)~%", k, u, u2,
                 the-connection.connection-weight);
        finally
          #f;
        end for;
      finally
        #f;
      end for;
    finally
      #f;
    end for;
  end with-open-file;
end method print-weights;

define method print-weights-to-screen ()
  format-out("\n\nWeights:\n\n");
  for (layer = network then layer.net-next-layer, k = 1 then 1+(k),
       until output-layer?(layer))
    for (u = 0 then 1+(u), until u > layer.net-size)
      for (u2 = 1 then 1+(u2), until u2 > layer.net-next-layer.net-size)
        let the-connection = layer.net-connections[u, u2];
        format-out("%d\n", list(k, u, u2, the-connection.connection-weight));
      finally
        #f;
      end for;
    finally
      #f;
    end for;
  finally
    #f;
  end for;
end method print-weights-to-screen;

// 
//  Backprop.
//
define method backprop (infile, #key verbose)
  read-data(infile, verbose);
  init-network(verbose);
  learn(infile, verbose);
  print-weights(infile);
end method backprop;

