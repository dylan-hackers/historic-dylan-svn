module: cop

define class <predicting-cop> (<cop>)
  slot probability-map :: false-or(<vector>) = #f;
  slot my-target-node :: <node>;
end class <predicting-cop>;

define method choose-move(cop :: <predicting-cop>, world :: <world>)
  let possible-locations = generate-moves(cop.agent-player);
  possible-locations[random(possible-locations.size)];
end method choose-move;

define method make-plan(cop :: <predicting-cop>, world :: <world>) => (plan)
  if(world.world-robber | ~cop.probability-map)
    cop.probability-map := make(<vector>, size: maximum-node-id(), fill: 0.0s0);
    cop.probability-map[world.world-robber.player-location.node-id] := 1.0s0;
  else
    cop.probability-map := advance-probability-map(world, cop.probability-map);
  end if;

  for(a-cop in world.world-cops)
    cop.probability-map[a-cop.player-location.node-id] := 0
  end for;

  let sorted-nodes = sort(range(size: maximum-node-id()),
                                test: method(x, y)
                                          cop.probability-map[x] > cop.probability-map[y]
                                      end);

  cop.my-target-node := find-node-by-id(world, sorted-nodes[0]);

  let plan = make(<stretchy-vector>);

  for (other-cop in world.world-other-cops,
       target in subsequence(sorted-nodes, start: 1))
    if(cop.probability-map[target] = 0.0s0)
      target := sorted-nodes[0];
    end;
    let (distance, path) = distance(other-cop, find-node-by-id(world, target));
    if(distance > 0)
      add!(plan, generate-plan(world, other-cop, path[0]));
    else
      let possible-locations = generate-moves(other-cop);
      let new-location = possible-locations[random(possible-locations.size)];
      add!(plan, generate-plan(world, other-cop, new-location));
    end if;
  end for;
  plan
end method make-plan;

define method perceive-informs(informs, cop :: <predicting-cop>, world :: <world>)
end method perceive-informs;

define method advance-probability-map(world :: <world>, old-map :: <vector>)
 => (new-map :: <vector>);
  let new-map = make(<vector>, size: old-map.size);

  for(node in world.world-skeleton.world-nodes)
    new-map[node.node-id]
      := reduce1(\+, map(method(x)
                             (1.0s0 / x.moves-by-foot.size) * old-map[x.node-id]
                         end method,
                         node.moves-by-foot))
  end for;
  new-map
end method advance-probability-map;
