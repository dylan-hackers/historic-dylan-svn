Module:    rgsoftware-neural-net-internal
Synopsis:  C-FFI definitions for the NN50 DLL
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// C-FFI wrappers for the DLL functions. Each of these wrappers
// is an 'indirect' call so we don't need to link to an nn50.lib
// file. Instead the DLL is loaded and the functions are looked
// up dynamically using GetProcAddress.
define c-function train-indirect
  input output parameter dwArrayPointer :: <nn50-safe-array*>;
  input output parameter lpEpochs :: <c-long*>;
  input output parameter nResetWeights :: <c-int*>;
  input output parameter nLearningRate :: <c-double*>;
  input output parameter nMomentum :: <c-double*>;
  input output parameter nMaxNeurons :: <c-double*>;
  parameter lpCallBackOfEpoch :: <c-function-pointer>;
  parameter lpCallBackOfRMSError :: <c-function-pointer>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end c-function train-indirect;

define c-function input-relevance-indirect
  input output parameter dwFromArrayPointer :: <nn50-safe-array*>;
  input output parameter dwToArrayPointer :: <nn50-safe-vector*>;
  input output parameter lpEpochs :: <c-long*>;
  input output parameter nLearningRate :: <c-double*>;
  input output parameter nMomentum :: <c-double*>;
  input output parameter nMaxNeurons :: <c-double*>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end c-function input-relevance-indirect;

define c-function load-network-indirect
  input output parameter dwArrayPointer :: <nn50-safe-array*>;
  parameter lpFileName :: <c-string>;
  input output parameter nLearningRate :: <c-double*>;
  input output parameter nMomentum :: <c-double*>;
  input output parameter nMaxNeurons :: <c-double*>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end c-function load-network-indirect;

define c-function save-network-indirect
  parameter lpFileName :: <c-string>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end c-function save-network-indirect;

define c-function predict-indirect
  input output parameter dwArrayPointer :: <nn50-safe-array*>;
  input output parameter nLearningRate :: <c-double*>;
  input output parameter nMomentum :: <c-double*>;
  input output parameter nMaxNeurons :: <c-double*>;
  result value :: <c-double>;
  indirect: #t;
  c-modifiers: "__stdcall";
end c-function predict-indirect;

define c-function stop-processing-indirect
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end c-function stop-processing-indirect;

// Variables to hold values of the callback functions. This
// approach allows users to use any method, whether defined or
// anonymous, to pass as the Epoch or RMS callbacks. They are
// thread variables, meaning there values are thread local, so
// that they work correctly in a multithreaded application.
define thread variable *epoch-callback* = #f;
define thread variable *rms-callback* = #f;

// Implementations of the callback functions that punt to the
// methods contained in the *epoch-callback* and *rms-callback*
// variables.
define function dylan-epoch-callback(epoch :: <integer>) => (result :: <integer>)
  *epoch-callback*(epoch);
end function dylan-epoch-callback;

define function dylan-rms-callback(rms :: <double-float>) => (result :: <integer>)
  *rms-callback*(rms);
end function dylan-rms-callback; 

define C-callable-wrapper c-epoch-callback of dylan-epoch-callback
  parameter epoch   :: <c-long>;
  result value :: <c-int>;
  c-modifiers: "__stdcall";
end C-callable-wrapper;

define C-callable-wrapper c-rms-callback of dylan-rms-callback
  parameter rms   :: <c-double>;
  result value :: <c-int>;
  c-modifiers: "__stdcall";
end C-callable-wrapper;

// Contains the Windows Module handle for the DLL
define constant $nn50-module = LoadLibrary("nn50.dll");

// Get the procedure addresses for all the functions
// in the NN50 DLL.
define constant $train-address = GetProcAddress($nn50-module, "TRAIN");
define constant $input-relevance-address = GetProcAddress($nn50-module, "INPUTRELEVANCE");
define constant $load-network-address = GetProcAddress($nn50-module, "LOADNETWORK");
define constant $save-network-address = GetProcAddress($nn50-module, "SAVENETWORK");
define constant $predict-address = GetProcAddress($nn50-module, "PREDICT");
define constant $stop-processing-address = GetProcAddress($nn50-module, "STOPPROCESSING");

// Define wrapper methods that call the NN50 DLL functions
// using the indirect method.
define function train(
    dwArrayPointer, 
    lpEpochs, 
    nResetWeights, 
    nLearningRate, 
    nMomentum, 
    nMaxNeurons, 
    lpCallBackOfEpoch,
    lpCallBackOfRMSError)
  dynamic-bind(*epoch-callback* = lpCallBackOfEpoch, *rms-callback* = lpCallBackOfRMSError)
    let (result, safe-array) =
      train-indirect($train-address, 
                     dwArrayPointer, 
                     lpEpochs, 
                     nResetWeights, 
                     nLearningRate, 
                     nMomentum, 
                     nMaxNeurons, 
                     c-epoch-callback,
                     c-rms-callback); 
    values(result, safe-array);
  end dynamic-bind;
end function train;

define function input-relevance(
    dwFromArrayPointer,
    dwToArrayPointer,
    lpEpochs, 
    nLearningRate, 
    nMomentum, 
    nMaxNeurons)
    let (result, from-array, to-array) =
      input-relevance-indirect($input-relevance-address, 
                               dwFromArrayPointer,
                               dwToArrayPointer,
                               lpEpochs,
                               nLearningRate,
                               nMomentum,
                               nMaxNeurons);
    values(result, from-array, to-array);
end function input-relevance;

define function load-network(dwArrayPointer,
                             lpFileName,
                             nLearningRate,
                             nMomentum,
                             nMaxNeurons)
    let (result, array) =
      load-network-indirect($load-network-address,
                            dwArrayPointer,
                            lpFileName,
                            nLearningRate,
                            nMomentum,
                            nMaxNeurons);
      values(result, array);
end function load-network;

define function save-network(lpFileName)
  save-network-indirect($save-network-address, lpFileName)
end function save-network;

define function predict(dwArrayPointer,
                        nLearningRate,
                        nMomentum,
                        nMaxNeurons)
  let (result, array) =
    predict-indirect($predict-address, 
                     dwArrayPointer,
                     nLearningRate,
                     nMomentum,
                     nMaxNeurons);
    values(result, array);
end function predict;

define function stop-processing()
  stop-processing-indirect($stop-processing-address)
end function stop-processing;

