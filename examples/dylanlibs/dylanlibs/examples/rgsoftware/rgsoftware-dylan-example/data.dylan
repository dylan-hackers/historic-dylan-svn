Module:    rgsoftware-dylan-example
Synopsis:  Global data used by the program
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// The current neural net object being used.
define variable *nn* :: false-or(<neural-net>) = #f;

// The network learning rate (0.1 to 10)
define variable *learning-rate* = 4.5d0;

// The network momentum (0.1 to 10)
define variable *momentum* = 0.5d0;

// The maximum number of neurons
define variable *max-neurons* = 50d0;

// The RMS Error
define variable *rms-error* = 0d0;
