module: board

define class <package> (<object>)
  slot id :: <integer>;
  slot weight :: false-or(<integer>);
  slot x :: <integer>;
  slot y :: <integer>;

  slot dest-x :: false-or(<integer>);
  slot dest-y :: false-or(<integer>);

  slot carrier :: false-or(<robot>);
end class <package>;

define method at-destination?(p :: <package>) 
  p.dest-x & p.dest-y & p.x = p.dest-x & p.y = p.dest-y;
end method at-destination?;
