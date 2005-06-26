module: cop

define class <stupid-predicting-cop> (<predicting-cop>)
  slot my-target-node :: <node>;
end class;

define method choose-move(cop :: <stupid-predicting-cop>, world :: <world>)
  let (distance, path) = distance(cop.agent-player,
                                  cop.my-target-node);
  if (distance = 0)
    random-player-move(cop.agent-player);
  else
    path[0];
  end if;
end method choose-move;

define method make-plan(cop :: <stupid-predicting-cop>, world :: <world>) => (plan)
  let sorted-nodes
    = copy-sequence(sort(range(size: maximum-node-id()),
                         test: method(x, y)
                                   cop.probability-map[x] > cop.probability-map[y]
                               end), end: 5);

  let players = world.world-cops;
  let sorted-players = make(<stretchy-vector>);

  for(target in sorted-nodes)
    let remaining-players 
      = sort(players, 
             test: method(x, y)
                       distance(x, target.find-node-by-id) 
                         < distance(y, target.find-node-by-id)
                   end method);
    add!(sorted-players, remaining-players[0]);
    players := remove!(players, remaining-players[0]);
    if(remaining-players[0] = cop.agent-player)
      cop.my-target-node := find-node-by-id(sorted-nodes[0]);
    end if;
  end for;
      
  cop.my-target-node := find-node-by-id(sorted-nodes[0]);

  let plan = make(<stretchy-vector>);

  for (other-cop in sorted-players,
       target in sorted-nodes)
    if(cop.probability-map[target] = 0.0s0)
      target := sorted-nodes[0];
    end;
    let (distance, path) = distance(other-cop, find-node-by-id(target));
    if(distance > 0)
      add!(plan, generate-plan(world, other-cop, path[0]));
    else
      let new-location = random-player-move(other-cop);
      add!(plan, generate-plan(world, other-cop, new-location));
    end if;
  end for;
/*  for (p in plan)
    dbg("WORLD %s PLAN %s %s %s\n", world.world-number, p.plan-bot, p.plan-location.node-name, p.plan-type);
  end;
  for (i from 0 below maximum-node-id())
    if (cop.probability-map[i] > 0)
      dbg("%s %s\n", node-name(find-node-by-id(key-sequence(cop.probability-map)[i])), cop.probability-map[i]);
    end if;
  end for;*/
  plan
end method make-plan;
