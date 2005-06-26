module: cop
synopsis: 
author: 
copyright: 

define class <rookie-cop> (<cop>)
  slot info-robber-best-location :: false-or(<node>), init-keyword: robber-best-location:;
  slot bank-id :: <integer> = 0;
  slot local-map :: <vector>;
end class <rookie-cop>;

define method make-informs(cop :: <cop>, world :: <world>) => (informs);
  let infs = make(<stretchy-vector>);

  // Reset our map for this move.
  cop.local-map := make(<vector>, size: world.world-skeleton.world-nodes.size, fill: 0);

  dbg("Running make-informs. %=\n", cop.agent-player.player-location);
  
  let inf = make(<inform>,
                 inform-certainty: -100,
                 plan-bot: world.world-skeleton.robber-name,
                 plan-location: cop.agent-player.player-location,
                 plan-type: "robber",
                 plan-world: world.world-number + 1);
  add!(infs, inf);

  dbg("I never print.\n");

  let n-num = cop.agent-player.player-location.moves-by-foot.size;
  for (n :: <node> in cop.agent-player.player-location.moves-by-foot)
    dbg("Processing 1 away.\n");
    
    if (world.world-smell-distance = 1)
      let inf = make(<inform>,
                     inform-certainty: truncate/(100,n-num),
                     plan-bot: world.world-skeleton.robber-name,
                     plan-location: n,
                     plan-type: "robber",
                     plan-world: world.world-number + 1);
      add!(infs, inf);
    else
      let inf = make(<inform>,
                     inform-certainty: -100,
                     plan-bot: world.world-skeleton.robber-name,
                     plan-location: n,
                     plan-type: "robber",
                     plan-world: world.world-number + 1);
      add!(infs, inf);
    end if;

    dbg("Processing 2 away.\n");
    
    if (world.world-smell-distance = 2)
      let nn-num = n.moves-by-foot.size;
      for (nn in n.moves-by-foot)
        let inf = make(<inform>,
                       inform-certainty: truncate/(100,nn-num),
                       plan-bot: world.world-skeleton.robber-name,
                       plan-location: nn,
                       plan-type: "robber",
                       plan-world: world.world-number + 1);
        add!(infs, inf);
      end for;
    end if;
  end for;

  dbg("Finished unning make-informs.\n");
  
  infs;
end method make-informs;

define method perceive-informs(informs-dylan-is-stupid, cop :: <cop>, world :: <world>);
  for (from-message-inform :: <from-message-inform> in informs-dylan-is-stupid)
    for (inf :: <inform> in from-message-inform.informs)
      // Check the McGruff reports about the robber.
      cop.local-map[inf.plan-location.node-id] :=
        cop.local-map[inf.plan-location.node-id] + inf.inform-certainty;
      
      if (inf.plan-bot = world.world-skeleton.robber-name)
        if (inf.inform-certainty > 0)
          cop.info-robber-best-location := inf.plan-location;
          dbg("McGruff detected robber!\n");
        end if;
      end if;
    end for;
  end for;
end method perceive-informs;

define method make-plan(cop :: <rookie-cop>, world :: <world>) => (plan)
  let plan = make(<stretchy-vector>);

  let my-cop-player = world.world-cops[0];
  let copA-player = world.world-cops[1];
  let copB-player = world.world-cops[2];
  let copC-player = world.world-cops[3];
  let copD-player = world.world-cops[4];

  local method choose-move-random(cop-player, transport)
          let move = make(<move>,
                          target: cop-player.player-location,
                          transport: transport);
          let possible-locations = generate-moves(move, keep-current-transport: #t);
          let new-location = possible-locations[random(possible-locations.size)];
          add!(plan, generate-plan(world, cop-player, new-location));
        end;

  local method choose-move-to-target(cop-player, transport, target)
          let move = make(<move>,
                          target: cop-player.player-location,
                          transport: transport);
          let (rank, shortest-path)
            = distance(cop-player, target, source: move, keep-current-transport: #t);
          
          if (rank > 0)
            add!(plan, generate-plan(world, cop-player, shortest-path[0]));
          else
            let possible-locations = generate-moves(move, keep-current-transport: #t);
            let new-location = possible-locations[random(possible-locations.size)];
            add!(plan, generate-plan(world, cop-player, new-location));
          end if;
        end;
  
  // Do this as initialising step.
  if (world.world-number = 1)
    // Initialise info.
    cop.info-robber-best-location := world.world-robber.player-location;

    choose-move-to-target(my-cop-player, "cop-foot", cop.info-robber-best-location);
    choose-move-to-target(copA-player, "cop-car", cop.info-robber-best-location);
    choose-move-to-target(copB-player, "cop-foot", cop.info-robber-best-location);
    choose-move-to-target(copC-player, "cop-car", cop.info-robber-best-location);
    choose-move-to-target(copD-player, "cop-foot", cop.info-robber-best-location);
  else
    // If someone reached robots location, reset it.
    if (my-cop-player.player-location = cop.info-robber-best-location |
          copA-player.player-location = cop.info-robber-best-location |
          copB-player.player-location = cop.info-robber-best-location |
          copC-player.player-location = cop.info-robber-best-location |
          copD-player.player-location = cop.info-robber-best-location)
      cop.info-robber-best-location := #f;
    end if;
    
    // If robber showed up, go there!
    dbg("In world %d world-robber is %=.\n", world.world-number, world.world-robber);
    if (world.world-robber ~= #f)
      cop.info-robber-best-location := world.world-robber.player-location;
      
      choose-move-to-target(my-cop-player, my-cop-player.player-type,
                            cop.info-robber-best-location);
      choose-move-to-target(copA-player, copA-player.player-type,
                            cop.info-robber-best-location);
      choose-move-to-target(copB-player, copB-player.player-type,
                            cop.info-robber-best-location);
      choose-move-to-target(copC-player, copC-player.player-type,
                            cop.info-robber-best-location);
      choose-move-to-target(copD-player, copD-player.player-type,
                            cop.info-robber-best-location);
    else
      // Check for evidence for the latest location of robot.
      let best-evidence-world = world.world-number - 8; // ignore old evidence
      for (e in world.world-evidences)
        if (e.evidence-world >= best-evidence-world)
          cop.info-robber-best-location := e.evidence-location;
          best-evidence-world := e.evidence-world;
        end if;
      end for;

      // Choose the best probably location based on local-map.
      let best-node-id = -1;
      let best-node-val = 0;
      for (i from 0 below cop.local-map.size)
        if (cop.local-map[i] >= best-node-val)
          best-node-val = cop.local-map[i];
          best-node-id = i;
        end if;
      end for;

      // If we smell it, call others to come.
      if (best-node-val > -1)
        choose-move-to-target(my-cop-player, my-cop-player.player-type,
                              world.world-skeleton.world-nodes[best-node-id]);
        choose-move-to-target(copA-player, copA-player.player-type,
                              world.world-skeleton.world-nodes[best-node-id]);
        choose-move-to-target(copB-player, copB-player.player-type,
                              world.world-skeleton.world-nodes[best-node-id]);
        choose-move-to-target(copC-player, copC-player.player-type,
                              world.world-skeleton.world-nodes[best-node-id]);
        choose-move-to-target(copD-player, copD-player.player-type,
                              world.world-skeleton.world-nodes[best-node-id]);
      else
        if (world.world-smell-distance > 1)
          choose-move-random(my-cop-player, my-cop-player.player-type);
          choose-move-to-target(copA-player, copA-player.player-type,
                                my-cop-player.player-location);
          choose-move-to-target(copB-player, copB-player.player-type,
                                my-cop-player.player-location);
          choose-move-to-target(copC-player, copC-player.player-type,
                                my-cop-player.player-location);
        elseif (cop.info-robber-best-location ~= #f)
          choose-move-to-target(my-cop-player, my-cop-player.player-type,
                                cop.info-robber-best-location);
          choose-move-to-target(copA-player, copA-player.player-type,
                                cop.info-robber-best-location);
          choose-move-to-target(copB-player, copB-player.player-type,
                                cop.info-robber-best-location);
          choose-move-to-target(copC-player, copC-player.player-type,
                                cop.info-robber-best-location);
        else
          choose-move-random(my-cop-player, my-cop-player.player-type);
          choose-move-random(copA-player, copA-player.player-type);
          choose-move-random(copB-player, copB-player.player-type);
          choose-move-random(copC-player, copC-player.player-type);
        end if;

        // Patrol banks otherwise.
        if (world.world-banks[cop.bank-id].bank-location =
              copD-player.player-location)
          cop.bank-id := cop.bank-id + 1;
          if (cop.bank-id = 6)
            cop.bank-id := 0
          end if;
        end if;
        choose-move-to-target(copD-player, copD-player.player-type,
                              world.world-banks[cop.bank-id].bank-location);
      end if;
    end if;
  end if;
  
  plan
end method make-plan;

define method perceive-plans(plans, cop :: <cop>, world :: <world>);
end method perceive-plans;

define method make-vote(cop :: <cop>, world :: <world>) => (vote);
  concatenate(list(world.world-my-player), world.world-other-cops);
end method make-vote;

define method perceive-vote(vote, cop :: <cop>, world :: <world>);
end method perceive-vote;

define method choose-move(cop :: <rookie-cop>, world :: <world>)
  if (cop.info-robber-best-location ~= #f)
    let (rank, shortest-path)
      = distance(cop.agent-player, cop.info-robber-best-location);
    
    if (rank > 0)
      dbg("shortest-path[0] = %= and rank = %=\n", shortest-path[0], rank);
      shortest-path[0];
    else
      let possible-locations = generate-moves(cop.agent-player);
      possible-locations[random(possible-locations.size)];
    end if;
  else
    let possible-locations = generate-moves(cop.agent-player);
    possible-locations[random(possible-locations.size)];
  end if;
end method choose-move;

drive-agent(make(<rookie-cop>), *standard-input*, *standard-output*);
