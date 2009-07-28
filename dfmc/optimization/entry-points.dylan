Synopsis: upgrading of calls to more efficient entry points
Author:   Jonathan Bachrach, Keith Playford, Paul Haahr
Module:   dfmc-optimization
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Simple entry-point pass

// define compilation-pass analyze-calls,
//   visit: computations,
//   optimization: low,
//   before: single-value-propagation;

define method analyze-calls (c :: <computation>)
  #f
end method;

define method analyze-calls (c :: <primitive-call>)
  maybe-optimize-function-call(c, c.primitive, c.arguments);
end method;

define method analyze-calls (c :: <function-call>)
  // If what's being called is not a valid function, or there is some
  // clear incompatibility between the arguments and the function,
  // don't attempt to do anything with the call.
  let ef = call-effective-function(c);
  let call-ok? = maybe-check-function-call(c);
  if (*call-upgrading?*)
    if (call-ok? & ef)
      instance?(ef, <&lambda>) & ensure-method-model(ef);
      maybe-upgrade-call(c, ef) |
	maybe-optimize-function-call
	  (c, call-effective-function(c), c.arguments)
    else 
      #f
    end if
  else 
    #f
  end if;
end method;

