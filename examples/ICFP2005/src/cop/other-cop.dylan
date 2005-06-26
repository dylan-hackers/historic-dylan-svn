module: cop

define class <predicting-cop> (<cop>)
  slot probability-map :: false-or(<vector>) = #f;
  slot my-target-node :: <node>;
  slot last-precise-info :: <integer> = 1;
end class <predicting-cop>;

define method choose-move(cop :: <predicting-cop>, world :: <world>)
  let (distance, path) = distance(cop.agent-player,
                                  cop.my-target-node);
  //dbg("CHMOVE %s %s %s\n", cop.my-target-node.node-name, distance,
  //    cop.agent-player.player-location.node-name);
  if (distance = 0)
    random-player-move(cop.agent-player);
  else
    path[0];
  end if;
end method choose-move;

define method make-plan(cop :: <predicting-cop>, world :: <world>) => (plan)
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
      let possible-locations = generate-moves(other-cop);
      let new-location = possible-locations[random(possible-locations.size)];
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

define method consider-evidence (evidence, number, cop)
  let mymap = make(<vector>, size: maximum-node-id(), fill: 0.0s0);
  mymap[evidence.evidence-location.node-id] := 1.0s0;
  for (i from evidence.evidence-world + 2
         below number by 2)
    mymap := advance-probability-map(mymap);
    //XXX set bank-location and cop-location in worlds to 0.0s0
  end;
  cop.probability-map := map(\*, mymap, cop.probability-map);
  normalize!(cop.probability-map);
end method;

define method make-informs (cop :: <predicting-cop>, world :: <world>)
 => (object)
  let res = #();
  if(world.world-robber | ~cop.probability-map)
    cop.last-precise-info := world.world-number;
    cop.probability-map := make(<vector>, size: maximum-node-id(), fill: 0.0s0);
    cop.probability-map[world.world-robber.player-location.node-id] := 1.0s0;
  else
    cop.probability-map := advance-probability-map(cop.probability-map);
    for (bank in world.world-banks)
      //we'll receive a message if robber is at the bank
      cop.probability-map[bank.bank-location.node-id] := 0.0s0;
    end for;
    for(a-cop in world.world-cops)
      cop.probability-map[a-cop.player-location.node-id] := 0.0s0;
    end for;
    normalize!(cop.probability-map);

    if (world.world-evidences.size > 0)
      let newest-evidence
        = first(sort(world.world-evidences,
                     test: method(x, y)
                               x.evidence-world < y.evidence-world;
                           end method));
      if (newest-evidence.evidence-world > cop.last-precise-info)
        res := add!(res, generate-inform
                      (world,
                       newest-evidence.evidence-location,
                       100,
                       number: newest-evidence.evidence-world));
        dbg("NEWEST EVIDENCE: loc: %s in world: %s current world: %s\n",
            newest-evidence.evidence-location.node-name,
            newest-evidence.evidence-world,
            world.world-number);

        consider-evidence(newest-evidence,
                          world.world-number,
                          cop);
      end if;
    end if;

    if (world.world-smell-distance > 0)
      let (first-nodes, second-nodes)
        = smelled-nodes-aux(cop.agent-player);
      let prob-map = make(<vector>, size: maximum-node-id(),
                          fill: 0.0s0);

      local method set-map(nodes, value)
              for (node in nodes)
                prob-map[node.node-id] := value;
              end for;
            end;
      let nodes = concatenate(first-nodes, second-nodes);

      if (world.world-smell-distance = 1)
        set-map(first-nodes, 1.0s0 / first-nodes.size);
        set-map(second-nodes, 0.0s0);
      elseif (world.world-smell-distance = 2)
        set-map(second-nodes, 1.0s0 / second-nodes.size);
        set-map(first-nodes, 0.0s0);
      end if;
      
      dbg("SMELL %s world: %s size: %s loc: %s\n",
          world.world-smell-distance,
          world.world-number,
          nodes.size,
          cop.agent-player.player-location.node-name);
      for (e in nodes)
        dbg("SMELL %s\n", e.node-name);
      end for;
      res := concatenate(res, generate-informs(world,
                                               prob-map,
                                               nodes));
      cop.probability-map := map(\*, prob-map, cop.probability-map);
      normalize!(cop.probability-map);
    else
      //set all probabilities in smell-reach to 0 (we know, there's
      //no robber around)
      let nodes = smelled-nodes(cop.agent-player);
      for (node in nodes)
        cop.probability-map[node.node-id] := 0.0s0;
      end for;
      res := concatenate(res, generate-informs(world,
                                               cop.probability-map,
                                               nodes));

      normalize!(cop.probability-map);
    end;
  end if;

  res;
end method;

define method perceive-informs(information, cop :: <predicting-cop>, world :: <world>)
  unless (world.world-robber)
    let newest-info = #f;
    for (inform in information)
      unless (inform.sender = world.world-my-player.player-name)
        let worlds = reverse(sort(remove-duplicates(map(plan-world,
                                                        inform.informs))));

        for (number in worlds)
          block(continue)
            if(number <= cop.last-precise-info)
              continue();
            end if;
            if(number > world.world-number)
              continue();
            end if;
            let infos = choose(method(x)
                                   x.plan-world = number
                               end, inform.informs);
/*            do(method(x)
                   dbg("INFORM from %s bot %s world %s loc %s val %s\n",
                       inform.sender,
                       x.plan-bot,
                       x.plan-world,
                       x.plan-location.node-name,
                       x.inform-certainty)
               end,
               infos);*/
            
            if (number = world.world-number)

              let prob-map = make(<vector>,
                                  size: maximum-node-id(),
                                  fill: 0.5s0);

              for (info in infos)
                prob-map[info.plan-location.node-id]
                  := (info.inform-certainty + 100.0s0) / 200.0s0;
              end for;
            
              cop.probability-map := map(\*, prob-map, cop.probability-map);
              normalize!(cop.probability-map);
              continue();
            end if;
            if (infos[0].inform-certainty = 100)
              cop.last-precise-info := number;
              newest-info := infos[0];
            end if;
          end block;
        end for;
      end unless;
    end for;
    if (newest-info)
      consider-evidence(make(<evidence>,
                             location: newest-info.plan-location,
                             world: newest-info.plan-world),
                        world.world-number,
                        cop);
     /* dbg("NEWEST EVIDENCE FROM SOMEONE: loc: %s in world: %s current world: %s\n",
          newest-info.plan-location.node-name,
          newest-info.plan-world,
          world.world-number);*/
    end if;
  end unless;
end method perceive-informs;

define method advance-probability-map(old-map :: <vector>)
 => (new-map :: <vector>);
  let new-map = make(<vector>, size: old-map.size);

  for(node in *world-skeleton*.world-nodes)
    new-map[node.node-id]
      := reduce1(\+, map(method(x)
                             (1.0s0 / x.moves-by-foot.size) * old-map[x.node-id]
                         end method,
                         node.moves-by-foot))
  end for;
  new-map
end method advance-probability-map;
