module: board

define class <robot> (<object>)
  class slot capacity :: <integer>;
  slot id :: <integer>,
    required-init-keyword: id:;
  slot location :: <point>,
    required-init-keyword: location:;
  slot money :: false-or(<integer>),
    init-keyword: money:,
    init-value: #f;
  slot inventory :: /*limited( */ <vector> /*, of: <package>)*/ ,
    init-keyword: inventory:,
    init-value: make(/*limited(*/ <vector> /*, of: <package>)*/ , size: 0);
  slot score :: <integer>,
    init-keyword: score:,
    init-value: 0;
end;

define method capacity-left(r :: <robot>)
 => (w :: <integer>)
  if (r.capacity | ~every?(identity, r.inventory))
    r.capacity - reduce(\+, 0, map(weight, r.inventory));
  else
    #f
  end if;
end method capacity-left;

define method copy-robot( robot :: <robot>,
                         #key new-id,
                         new-score,
                         new-location,
                         new-money,
                         new-inventory ) 
  => ( r :: <robot> )
  let id = new-id | robot.id;
  let location = new-location | robot.location;
  let money = new-money | robot.money;
  let inventory = new-inventory | robot.inventory;
  let score = new-score | robot.score;
  make(<robot>,
	   id: id,
	   location: location,
       money: money,
       score: score,
       inventory: inventory);
end method copy-robot;

define method print-object(robot :: <robot>, stream :: <stream>)
 => ();
  format(stream, "{robot id: %d, capacity: %d, location: %=, money: %=, inventory: %=, score: %d}",
         robot.id, robot.capacity, robot.location, robot.money, robot.inventory.size, robot.score);
end;

