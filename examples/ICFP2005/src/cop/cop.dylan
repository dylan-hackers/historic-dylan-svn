module: cop
synopsis: 
author: 
copyright: 

define class <rookie-cop> (<cop>)
  slot info-robber-best-location :: <node>, init-keyword: robber-best-location:;
end class <rookie-cop>;

define method choose-move(cop :: <rookie-cop>, world :: <world>)
  let (rank, shortest-path)
    = distance(cop.agent-player, cop.info-robber-best-location);

  if (rank > 0)
    dbg("shortest-path[0] = %= and rank = %=\n", shortest-path[0], rank);
    shortest-path[0];
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
        if (inf.inform-certainty = 100)
          cop.info-robber-best-location := inf.plan-location;
          dbg("McGruff detected robber!\n");
        end if;
      end if;
    end for;
  end for;
end method perceive-informs;

define method make-plan(cop :: <rookie-cop>, world :: <world>) => (plan)
  let plan = make(<stretchy-vector>);

  let my-cop = world.world-cops[0];
  let copA = world.world-cops[1];
  let copB = world.world-cops[2];
  let copC = world.world-cops[3];
  let copD = world.world-cops[4];

  // Do this as initialising step.
  if (world.world-number = 1)

    // Initialise info.
    cop.info-robber-best-location := world.world-robber.player-location;
    
    // My cop gets foot.
    let possible-locations = generate-moves(my-cop);
    let new-location = possible-locations[random(possible-locations.size)];
    while (new-location.transport ~= "cop-foot")
      new-location := possible-locations[random(possible-locations.size)];
    end while;
    add!(plan, generate-plan(world, my-cop, new-location));

    // copA gets car.
    let possible-locations = generate-moves(copA);
    let new-location = possible-locations[random(possible-locations.size)];
    while (new-location.transport ~= "cop-car")
      new-location := possible-locations[random(possible-locations.size)];
    end while;
    add!(plan, generate-plan(world, copA, new-location));

    // copB gets car.
    let possible-locations = generate-moves(copB);
    let new-location = possible-locations[random(possible-locations.size)];
    while (new-location.transport ~= "cop-car")
      new-location := possible-locations[random(possible-locations.size)];
    end while;
    add!(plan, generate-plan(world, copB, new-location));

    // copC gets foot.
    let possible-locations = generate-moves(copC);
    let new-location = possible-locations[random(possible-locations.size)];
    while (new-location.transport ~= "cop-foot")
      new-location := possible-locations[random(possible-locations.size)];
    end while;
    add!(plan, generate-plan(world, copC, new-location));

    // copD gets foot.
    let possible-locations = generate-moves(copD);
    let new-location = possible-locations[random(possible-locations.size)];
    while (new-location.transport ~= "cop-foot")
      new-location := possible-locations[random(possible-locations.size)];
    end while;
    add!(plan, generate-plan(world, copD, new-location));
  else
    // Check for evidence.
    for (e in world.world-evidences)
      cop.info-robber-best-location := e.evidence-location;
    end for;
    
    for (cop-i in world.world-cops)
      let (rank, shortest-path)
        = distance(cop-i, cop.info-robber-best-location);

      if (rank > 0)
        add!(plan, generate-plan(world, cop-i, shortest-path[0]));
      else
        let possible-locations = generate-moves(cop-i);
        let new-location = possible-locations[random(possible-locations.size)];
        add!(plan, generate-plan(world, cop-i, new-location));
      end if;
    end for;
  end if;
  
  plan
end method make-plan;

drive-agent(make(<rookie-cop>), *standard-input*, *standard-output*);
