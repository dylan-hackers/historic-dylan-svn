module: board

define abstract class <robot>(<object>)
  slot id;
  slot capacity :: <integer>;

  slot x :: <integer>;
  slot y :: <integer>;
  slot money :: <integer>;
  slot inventory :: limited(<vector>, of: <package>);
end;

define method capacity-left(r :: <robot>)
 => (w :: <integer>)
  r.capacity - reduce1(\+, map(weight, r.inventory));
end method capacity-left;
