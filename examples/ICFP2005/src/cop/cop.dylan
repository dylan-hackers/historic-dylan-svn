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

    local method choose-move(cop, transport)
            let move = make(<move>,
                            target: cop.player-location,
                            transport: transport);
            let possible-locations = generate-moves(move, keep-current-transport: #t);
            let new-location = possible-locations[random(possible-locations.size)];
            add!(plan, generate-plan(world, cop, new-location));
          end;

    choose-move(my-cop, "cop-foot");
    choose-move(copA, "cop-car");
    choose-move(copB, "cop-car");
    choose-move(copC, "cop-foot");
    choose-move(copD, "cop-foot");
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
