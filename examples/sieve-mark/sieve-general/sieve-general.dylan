Module:    sieve-general
Author:    Eric Kidd <eric.kidd@pobox.com>
Copyright: Public Domain
Synopsis:  A benchmark using the "Sieve of Eratosthenes" to calculate all
           the primes within a given range. This version allows the size
           to be specified on the command line, which breaks our bounds-check
           avoidance.

define constant <int-vector> = limited(<simple-vector>, of: <integer>);

//define variable *limit* = 5000000;
define variable *limit* = 500000;

define method main(name, #rest args)
  //if (args.size ~= 1)
  //  format("Usage: sieve-general limit\n");
  //  exit(exit-code: 0);
  //end if;
  //let limit = as(<integer>, args.first);
  let limit :: <integer> = *limit*;

  // Element i in 'vec' corresponds to the integer i+1. We eventually want
  // a zero in element i if i+1 is prime, and the value of i+1 otherwise.
  //
  // The type declaration below is necessary until I fix type inference on
  // the return type of make-limited-collection. Notice that Dylan insists
  // on initializing our buffer for us.
  let vec :: <int-vector> = make(<int-vector>, size: limit, fill: 0);

  // Insert our numbers first. We could have just used 'fill: 1' above,
  // but that would have artificially enhanced our performance relative to
  // C.
  for (i from 0 below limit)
    vec[i] := i + 1;
  end for;

  // We know 1 isn't prime.
  vec[0] := 0;
  let prime-count = 0;

  // Sieve for the rest.
  for (i from 1 below limit)
    // We found a new prime!
    if (vec[i] ~= 0)
      prime-count := prime-count + 1;
      let prime = i + 1;
      // Eliminate our multiples.
      for (j from (i + prime) below limit by prime)
	vec[j] := 0;
      end for;
    end if;
  end for;

  // Print our findings.
  format("There are %d primes less than or equal to %d.\n",
	 prime-count, limit);
end method main;
