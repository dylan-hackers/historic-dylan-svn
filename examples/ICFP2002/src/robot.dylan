module: board

define class <robot> (<object>)
  slot id :: <integer>,
    required-init-keyword: id:;
  slot location :: <point>,
    required-init-keyword: location:;
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

define method copy-robot( robot :: <robot>, #key id, location, capacity, money, inventory ) 
  => ( r :: <robot> )
  make(<robot>,
	   id: id | robot.id,
	   location: location | robot.location,
	   capacity: capacity | robot.capacity,
       money: money | robot.money,
       inventory: inventory | robot.inventory);
end method copy-robot;
