module: cop
synopsis: 
author: 
copyright: 

define class <rookie-cop> (<cop>)
  slot info-robber-best-location :: false-or(<node>), init-keyword: robber-best-location:;
  slot bank-id :: <integer> = 0;
end class <rookie-cop>;

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

define method perceive-informs(informs-dylan-is-stupid, cop :: <cop>, world :: <world>);
  for (from-message-inform :: <from-message-inform> in informs-dylan-is-stupid)
    for (inf :: <inform> in from-message-inform.informs)
      // Check the McGruff reports about the robber.
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
          let (rank, shortest-path)
            = distance(cop-player, target);
          
          if (rank > 0)
            add!(plan, generate-plan(world, cop-player, shortest-path[0]));
          else
            let move = make(<move>,
                            target: cop-player.player-location,
                            transport: transport);
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
    choose-move-to-target(copA-player, "cop-foot", cop.info-robber-best-location);
    choose-move-to-target(copB-player, "cop-car", cop.info-robber-best-location);
    choose-move-to-target(copC-player, "cop-foot", cop.info-robber-best-location);
    choose-move-to-target(copD-player, "cop-car", cop.info-robber-best-location);
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
      
      // If we smell it, call others to come.
      if (world.world-smell-distance = 1)
        choose-move-random(my-cop-player, my-cop-player.player-type);
        choose-move-to-target(copA-player, copA-player.player-type,
                              my-cop-player.player-location);
        choose-move-to-target(copB-player, copB-player.player-type,
                              my-cop-player.player-location);
        choose-move-to-target(copC-player, copC-player.player-type,
                              my-cop-player.player-location);
        choose-move-to-target(copD-player, copD-player.player-type,
                              my-cop-player.player-location);
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

drive-agent(make(<rookie-cop>), *standard-input*, *standard-output*);
