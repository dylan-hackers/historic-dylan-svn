module: board

define class <robot> (<object>)
  slot id :: <integer>,
    required-init-keyword: id:;
  slot x :: <integer>,
    required-init-keyword: x:;
  slot y :: <integer>,
    required-init-keyword: y:;
  slot capacity :: false-or(<integer>),
    init-keyword: capacity:,
    init-value: #f;
  slot money :: false-or(<integer>),
    init-keyword: money:,
    init-value: #f;
  slot inventory :: limited(<vector>, of: <package>),
    init-keyword: inventory:,
    init-value: make(limited(<vector>, of: <package>), size: 0);
end;

define method capacity-left(r :: <robot>)
 => (w :: <integer>)
  if (r.capacity | ~every?(identity, r.inventory))
    r.capacity - reduce1(\+, map(weight, r.inventory));
  else
    #f
  end if;
end method capacity-left;
