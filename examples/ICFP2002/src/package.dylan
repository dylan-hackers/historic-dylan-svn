module: board

define class <package> (<object>)
  slot id :: <integer>, init-keyword: id:;
  slot weight :: false-or(<integer>), init-keyword: weight:;
  slot location :: false-or(<point>), init-keyword: location:;

  slot dest :: false-or(<point>), init-keyword: dest:;

  slot carrier :: false-or(<robot>);
end class <package>;

define method at-destination?(p :: <package>) 
  p.dest = p.location;
end method at-destination?;


