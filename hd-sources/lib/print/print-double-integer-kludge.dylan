Module:    print-internals
Author:    Gary Palter
Synopsis:  KLUDGE: Temporary method until division is implemented for <double-integer>
Copyright: 1997 The Harlequin Group Limited. All rights reserved.

define sealed method print-object
    (object :: <double-integer>, stream :: <stream>) => ()
  write(stream, "#ex");
  write(stream,
	copy-sequence(machine-word-to-string(%double-integer-high(object)), start: 2));
  write(stream, copy-sequence(machine-word-to-string(%double-integer-low(object)), start: 2))
end method;
