module: board

define class <package> (<object>)
  slot id :: <integer>;
  slot weight :: <integer>;
  slot x :: <integer>;
  slot y :: <integer>;

  slot dest-x :: <integer>;
  slot dest-y :: <integer>;

  slot carrier :: false-or(<robot>);
end class <package>;