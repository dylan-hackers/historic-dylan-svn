module: board

define class <package> (<object>)
  slot id :: <integer>;
  slot weight :: false-or(<integer>);
  slot location :: <point>;

  slot dest :: false-or(<point>);

  slot carrier :: false-or(<robot>);
end class <package>;

define method at-destination?(p :: <package>) 
  p.dest = p.location;
end method at-destination?;
