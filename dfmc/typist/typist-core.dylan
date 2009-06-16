Module:    DFMC-Typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method constant-value? 
  (ref :: <object-reference>)
   => (constant-value? :: <boolean>, constant-value)
  // Extract the constant from an <object-reference>.
  values(#t, reference-value(ref))
end method;

define sideways method constant-value? 
  (ref :: <defined-constant-reference>)
   => (constant-value? :: <boolean>, constant-value)
  // Extract the constant from an <defined-constant-reference>.
  // TODO: DOESN'T HANDLE FALSE
  let value = computation-value(ref);
  if (value)
    let (inlineable?, inline-value) = inlineable?(value);
    if (inlineable?)
      values(#t, inline-value)
    else
      values(#f, #f)
    end if
  else
    values(#f, #f)
  end if;
end method;

define sideways method constant-value?
    (ref :: <temporary>) => (constant-value? :: <boolean>, constant-value)
  if (instance?(ref.generator, <temporary-transfer>))
    //borrowed from fast-constant-argument-value? in constant-folding
    constant-value?(ref.generator.computation-value)
  elseif (instance?(ref.generator, <make-closure>))
    //borrowed from fast-constant-value? - but should actually be superfluous
    //(next case, invoking typist, should cover this special case)
    let m = computation-closure-method(ref.generator);
    if (^function-signature(m))
      values(#t, m)
    else
      values(#f, #f)
    end
  else
    // If this temporary is estimated as a singleton, extract the constant.
    let type = type-estimate(ref);
    if (instance?(type, <&singleton>))
      values(#t, type.^singleton-object)
    else
      values(#f, #f)
    end
  end
end method;

define sideways method constant-value?
  (ref :: <value-reference>) => (constant-value? :: <boolean>, constant-value)
  // Other kinds of <value-reference>s are not constants.
  values(#f, #f)
end method;

