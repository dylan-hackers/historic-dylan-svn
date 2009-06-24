Module:       io-internals
Synopsis:     *standard-input*, *standard-output*, *standard-error*
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// From WINBASE.H
define constant $STD_INPUT_HANDLE  = -10;
define constant $STD_OUTPUT_HANDLE = -11;
define constant $STD_ERROR_HANDLE  = -12;

define method make-std-stream
    (std-handle :: <integer>, direction)
 => (stream :: <file-stream>);
  let handle
    = primitive-wrap-machine-word
        (primitive-cast-pointer-as-raw
           (%call-c-function ("GetStdHandle", c-modifiers: "__stdcall")
              (nStdHandle :: <raw-c-unsigned-long>) => (handle :: <raw-c-pointer>)
              (integer-as-raw(std-handle))
           end));
  make(<file-stream>,
       locator: handle, file-descriptor: handle, direction: direction)
end method;

define variable *standard-input* 
  = make-std-stream($STD_INPUT_HANDLE, #"input");

define variable *standard-output*
  = make-std-stream($STD_OUTPUT_HANDLE, #"output");

define variable *standard-error*
  = make-std-stream($STD_ERROR_HANDLE, #"output");
