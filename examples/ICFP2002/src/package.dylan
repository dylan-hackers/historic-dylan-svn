module: board

define class <package> (<object>)
  slot id :: <integer>, init-keyword: id:;
  slot weight :: false-or(<integer>), init-keyword: weight:;
  slot location :: false-or(<point>), init-keyword: location:;

  slot dest :: false-or(<point>), init-keyword: dest:;

  slot carrier :: false-or(<robot>), 
    init-keyword: carrier:,
    init-value: #f;
end class <package>;

define method at-destination?(p :: <package>) 
  debug("Package location: %=, package.carrier: %=\n", p.location, p.carrier);
  p.dest = p.location;
end method at-destination?;

define method copy-package( p :: <package>, 
	#key new-id,
	     new-weight,
	     new-location,
	     new-dest,
	     new-carrier ) => (r :: <package>)
  let id = new-id | p.id;
  let weight = new-weight | p.weight;
  let location = new-location | p.location;
  let dest = new-dest | p.dest;
  let carrier = new-carrier | p.carrier;
  make(<package>,
	   id: id,
	   weight: weight,
       location: location,
       dest: dest,
	   carrier: carrier);
end method copy-package;


define method print-object (p :: <package>, s :: <stream>) => ()
  format(s, "{<package> id: %d, weight: %=, location: %=, dest: %=, carrier: %=}",
         p.id, p.weight, p.location, p.dest, p.carrier & p.carrier.id);
end method print-object;


