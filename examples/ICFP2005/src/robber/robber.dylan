module: robber
synopsis: 
author: 
copyright: 

define class <random-walk-robber> (<robber>)
  slot bank-id :: <integer> = 0;
end class <random-walk-robber>;

define method choose-move(robber :: <random-walk-robber>, world :: <world>)
 => (move);
  let (rank, shortest-path)
    = distance(robber.agent-player,
               world.world-banks[robber.bank-id].bank-location);
  if (rank > 0)
    shortest-path[0];
  else
    robber.bank-id := modulo(robber.bank-id + 1, 6);
    choose-move(robber, world);
  end if;
end method choose-move;
    
define function main(name, arguments)
  let robber = make(<random-walk-robber>);
  drive-agent(robber, *standard-input*, *standard-output*);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
