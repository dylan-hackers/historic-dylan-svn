module: robber
synopsis: 
author: 
copyright: 

define class <random-walk-robber> (<robber>)
//    slot bank-id :: <integer> = 0;
end class <random-walk-robber>;

define method choose-move(robber :: <random-walk-robber>, world :: <world>)
 => (move);
/*
  for (pl in world.world-cops)
    dbg("%s %s\n", pl.player-name, pl.player-location.node-name)
  end for;
  let (rank, shortest-path)
    = distance(robber.agent-player,
               world.world-banks[robber.bank-id].bank-location);
  if (rank > 0)
    shortest-path[0];
  else
    robber.bank-id := modulo(robber.bank-id + 1, 6);
    choose-move(robber, world);
  end if;
*/
    
    let distances =
        map(method(bank)
                let (rank, route) =
                    distance(robber.agent-player, bank.bank-location);
                pair(rank, route)
            end,
            world.world-banks);
    
    let best-bank =
        reduce1(method(rank-route-pair0, rank-route-pair1)
                    if (rank-route-pair0.head < rank-route-pair1.head)
                        rank-route-pair0;
                    else
                        rank-route-pair1;
                    end if;
                end,
                distances);
    
    if (size(best-bank.tail) > 0)
        best-bank.tail[0];
    else
        robber.agent-player.player-location;
    end if;
    
end method choose-move;
    
define function main(name, arguments)
    let robber = make(<random-walk-robber>);
    drive-agent(robber, *standard-input*, *standard-output*);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
