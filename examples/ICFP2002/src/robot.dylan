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
  slot inventory :: /*limited( */ <vector> /*, of: <package>)*/ ,
    init-keyword: inventory:,
    init-value: make(/*limited(*/ <vector> /*, of: <package>)*/ , size: 0);
end;

define method capacity-left(r :: <robot>)
 => (w :: <integer>)
  if (r.capacity | ~every?(identity, r.inventory))
    r.capacity - reduce1(\+, map(weight, r.inventory));
  else
    #f
  end if;
end method capacity-left;

define method copy-robot( robot :: <robot>,
                          #key new-id,
                               new-location,
                               new-capacity,
                               new-money,
                               new-inventory ) 
  => ( r :: <robot> )
  let id = new-id | robot.id;
  let location = new-location | robot.location;
  let money = new-money | robot.money;
  let inventory = new-inventory | robot.inventory;
  let capacity = new-capacity | robot.capacity;
  make(<robot>,
	   id: id,
	   location: location,
	   capacity: capacity,
       money: money,
       inventory: inventory);
end method copy-robot;
