module: cop
synopsis: 
author: 
copyright: 

define class <rookie-cop> (<cop>)
end class <rookie-cop>;

define method choose-move(cop :: <rookie-cop>, world :: <world>)
  let possible-locations = generate-moves(cop.agent-player);
  possible-locations[random(possible-locations.size)];
end method choose-move;

define method make-plan(cop :: <rookie-cop>, world :: <world>) => (plan)
  let plan = make(<stretchy-vector>);

  for (cop in world.world-other-cops)
    let possible-locations = generate-moves(cop);
    let new-location = possible-locations[random(possible-locations.size)];
    add!(plan, generate-plan(world, cop, new-location));
  end for;
  plan
end method make-plan;

drive-agent(make(<rookie-cop>), *standard-input*, *standard-output*);
