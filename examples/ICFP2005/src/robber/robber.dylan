module: robber
synopsis: 
author: 
copyright: 

define class <random-walk-robber> (<robber>)
//    slot bank-id :: <integer> = 0;
end class <random-walk-robber>;

define method random-move(robber :: <random-walk-robber>,
                          world :: <world>,
                          dangerous-nodes) => (move);

    let possible-nodes = map(target, generate-moves(robber.agent-player));
    
    let possible-safe-nodes =
        choose(method(node) ~member?(node, dangerous-nodes) end,
               possible-nodes);
               
    if (size(possible-safe-nodes) > 0)
        possible-safe-nodes[random(size(possible-safe-nodes))];
    else
        possible-nodes[random(size(possible-nodes))];
    end;
    
end method random-move;

define method choose-move(robber :: <random-walk-robber>, world :: <world>)
 => (move);

    let destination = block(return)

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

        let dangerous-nodes =
            reduce1(union,
                    map(smelled-nodes,
                        world.world-cops));

        if (size(best-bank.tail) > 0)
        
            dbg("got a plan to get to the bank\n");
        
            let node = best-bank.tail[0].target;
            if (~member?(node, dangerous-nodes))
                return(node);
            else
                dbg("gotta move randomly\n");
                return(random-move(robber, world, dangerous-nodes));
            end if;
            
        else
        
            dbg("at the bank\n");
            
            let node = robber.agent-player.player-location;
            if (~member?(node, dangerous-nodes))
                return(node);
            else
                dbg("gotta move randomly\n");
                return(random-move(robber, world, dangerous-nodes));
            end;
            
       end if;
        
    end;

    make(<move>,
         target: destination,
         transport: "robber");
    
end method choose-move;
    
define function main(name, arguments)
    let robber = make(<random-walk-robber>);
    drive-agent(robber, *standard-input*, *standard-output*);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
