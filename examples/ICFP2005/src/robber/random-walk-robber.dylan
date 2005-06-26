module: random-walk-robber

define class <random-walk-robber> (<robber>)
    slot most-recently-robbed-bank :: false-or(<bank>) = #f;
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
                    if (most-recently-robbed-bank == bank)
                        pair(100000, route);
                    elseif (bank.bank-money == 0)
                        robber.most-recently-robbed-bank := bank;
                        pair(100000, route);
                    else
                        pair(rank, route);
                    end
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
                    map(method(cop)
                            let nodes = smelled-nodes(cop);
                            /*dbg("cop at %= in %= smells %=\n",
                                cop.player-location.node-name,
                                cop.player-type,
                                map(node-name, nodes));*/
                            let next-positions = map(target, generate-moves(cop));
                            let next-nodes =
                                reduce(method(nodes0, node)
                                           let fake-player = make(<player>,
                                               name: cop.player-name,
                                               location: node,
                                               type: cop.player-type);
                                           union(nodes0, smelled-nodes(fake-player));
                                       end,
                                       #(),
                                       next-positions);
                            union(nodes, next-nodes);
                        end,
                        world.world-other-cops));
        //dbg("dangerous-nodes: %=\n", dangerous-nodes);

        if (size(best-bank.tail) > 0)
        
            //dbg("got a plan to get to the bank\n");
        
            let node = best-bank.tail[0].target;
            //dbg("currently at: %=\n", robber.agent-player.player-location.node-name);
            //dbg("node on way to bank: %=\n", node.node-name);
            
            if (~member?(node, dangerous-nodes))
                return(node);
            else
                //dbg("gotta move randomly\n");
                return(random-move(robber, world, dangerous-nodes));
            end if;
        else
            //dbg("at the bank\n");
            let node = robber.agent-player.player-location;
            if (~member?(node, dangerous-nodes))
                return(node);
            else
                //dbg("gotta move randomly\n");
                return(random-move(robber, world, dangerous-nodes));
            end;
       end if;
    end;

    make(<move>,
         target: destination,
         transport: "robber");
    
end method choose-move;
