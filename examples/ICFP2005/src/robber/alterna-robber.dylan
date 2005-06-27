module: alterna-robber

define class <alterna-robber> (<robber>)
  slot most-recently-robbed-bank :: false-or(<bank>) = #f;
end class <alterna-robber>;

define method choose-move(robber :: <alterna-robber>, world :: <world>)
  let possible-nodes = map(target, generate-moves(robber.agent-player));

  let evasive-move = choose-evasive-move(possible-nodes, robber, world);
  let robbing-move = choose-robbing-move(robber, world);
  if (modulo(world.world-number, 23) = 4)
    choose-random-move(robber, world)
  else
    evasive-move | robbing-move | choose-random-move(robber, world)
  end if;
end method choose-move;

define method choose-evasive-move(possible-nodes,
                                  robber :: <alterna-robber>,
                                  world :: <world>)
  local method evasive-score(target-node)
          reduce1(min, map(rcurry(distance, target-node), world.world-cops))
        end;

  if(evasive-score(robber.agent-player.player-location) < 4)
    let sorted-nodes = sort(possible-nodes, 
                            test: method(x, y)
                                      x.evasive-score > y.evasive-score
                                  end);

    make(<move>, target: sorted-nodes[0], transport: "robber");
  else
    #f
  end
end method choose-evasive-move;

define method choose-robbing-move(robber :: <alterna-robber>, world :: <world>)
  let distances =
    map(method(bank)
            let (rank, route) =
            distance(robber.agent-player, bank.bank-location);
            if (bank.bank-money == 0)
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
  
  if (size(best-bank.tail) > 0)
    make(<move>, target: best-bank.tail[0].target, transport: "robber")
  end if;
end method choose-robbing-move;
        
define method choose-random-move(robber :: <alterna-robber>, world :: <world>)
  let possible-locations = generate-moves(robber.agent-player);
  possible-locations[random(possible-locations.size)];
end method choose-random-move;

