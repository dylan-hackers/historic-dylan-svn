module: cop
synopsis: 
author: 
copyright: 

define class <rookie-cop> (<cop>)
end class <rookie-cop>;

define class <info>
    slot info-robber-best-location :: <node>, required-init-keyword: robber-best-location;
end class <info>;

define variable info :: <info>;

define method choose-move(cop :: <rookie-cop>, world :: <world>)
  let possible-locations = generate-moves(cop.agent-player);
  possible-locations[random(possible-locations.size)];
end method choose-move;

define method make-plan(cop :: <rookie-cop>, world :: <world>) => (plan)
  let plan = make(<stretchy-vector>);

  let my-cop = world.world-players[0];
  let copA = world.world-players[1];
  let copB = world.world-players[2];
  let copC = world.world-players[3];
  let copD = world.world-players[4];

  // Do this as initialising step.
  if (world.world-number = 1)

    // Initialise info.
    info := make(<info>, robber-best-location: 
    
    // My cop gets foot.
    my-cop.player-type := "cop-foot";
    let possible-locations = generate-moves(my-cop);
    let new-location = possible-locations[random(possible-locations.size)];
    add!(plan, generate-plan(world, my-cop, new-location));

    // copA gets car.
    copA.player-type := "cop-car";
    possible-locations := generate-moves(copA);
    new-location := possible-locations[random(possible-locations.size)];
    add!(plan, generate-plan(world, copA, new-location));

    // copB gets car.
    copB.player-type := "cop-car";
    possible-locations := generate-moves(copB);
    new-location := possible-locations[random(possible-locations.size)];
    add!(plan, generate-plan(world, copB, new-location));

    // copC gets foot.
    copC.player-type := "cop-foot";
    possible-locations := generate-moves(copC);
    new-location := possible-locations[random(possible-locations.size)];
    add!(plan, generate-plan(world, copC, new-location));

    // copD gets foot.
    copD.player-type := "cop-foot";
    possible-locations := generate-moves(copD);
    new-location := possible-locations[random(possible-locations.size)];
    add!(plan, generate-plan(world, copD, new-location));
  else
    for (cop in world.world-players)
      let possible-locations = generate-moves(cop);
      let new-location = possible-locations[random(possible-locations.size)];
      add!(plan, generate-plan(world, cop, new-location));
    end for;
  end if;
  
  plan
end method make-plan;

drive-agent(make(<rookie-cop>), *standard-input*, *standard-output*);
