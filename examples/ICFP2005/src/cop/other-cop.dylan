module: cop

define class <predicting-cop> (<cop>)
  slot probability-map :: false-or(<vector>) = #f;
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

  let plan = make(<stretchy-vector>);

  for (cop in world.world-other-cops)
    let possible-locations = generate-moves(cop);
    let new-location = possible-locations[random(possible-locations.size)];
    add!(plan, generate-plan(world, cop, new-location));
  end for;
  plan
end method make-plan;

define method advance-probability-map(world :: <world>, old-map :: <vector>)
 => (new-map :: <vector>);
  let new-map = make(<vector>, size: old-map.size);

  for(node in world.world-skeleton.world-nodes)
    new-map[node.node-id]
      := reduce1(\+, map(method(x)
                             (1.0s0 / x.moves-by-foot) * old-map[x.node-id]
                         end method,
                         node.moves-by-foot))
  end for;
  new-map
end method advance-probability-map;
