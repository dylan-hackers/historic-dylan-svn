module: introspection
author: Scott McKay
copyright: 1996 The Harlequin Group Limited. All rights reserved.


///---*** Interim kludges for the native environment

define method class-name (object) => (not :: singleton(#f))
  #f
end method class-name;


/// function-name -- Interim Kludge.
define method function-name (fun :: <function>) => (name :: singleton(#f))
  #f
end method function-name;

define method slot-name (slot :: <slot-descriptor>) => (name);
  #f
end method slot-name;

define method slot-value (slot :: <slot-descriptor>, object) => (value, bound?);
  values(#f, #t)
end method slot-value;

define generic function-specializers (function :: <function>)
 => (sequence :: <sequence>);

define method function-specializers (function :: <method>)
 => (sequence :: <sequence>);
  #()
end method function-specializers;

define method function-specializers (function :: <function>)
 => (sequence :: <sequence>);
  #()
end method function-specializers;

