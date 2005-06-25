module: cop
synopsis: 
author: 
copyright: 

define constant <cop-type> = <string>;
define constant cop-foot :: <cop-type> = "cop-foot";
define constant cop-car :: <cop-type> = "cop-car";

define variable my-cop-type :: <cop-type> = cop-foot;
define variable my-cop-name :: <string> = "DyCop42";

define variable copA-name :: <string> = "";
define variable copA-type :: <cop-type> = cop-foot;
define variable copB-name :: <string> = "";
define variable copB-type :: <cop-type> = cop-foot;
define variable copC-name :: <string> = "";
define variable copC-type :: <cop-type> = cop-foot;
define variable copD-name :: <string> = "";
define variable copD-type :: <cop-type> = cop-foot;

define class <rookie-cop> (<cop>)
end class <rookie-cop>;

define method choose-move(cop :: <rookie-cop>, world :: <world>)
  let player = block(return)
                 for (player in world.world-players)
                   if (player.player-name = world.world-skeleton.my-name)
                     return(player);
                   end if;
                 end for;
               end block;

  let possible-locations = generate-moves(world, player);
  possible-locations[random(possible-locations.size)];
end method choose-move;

define method make-plan(cop :: <rookie-cop>, world :: <world>) => (plan)
  for (cop in world.world-players)
    if (cop.player-name ~= world.world-skeleton.my-name)
      if (copA-name = "")
        copA-name := cop.player-name;
      elseif (copB-name = "")
        copB-name := cop.player-name;
      elseif (copC-name = "")
        copC-name := cop.player-name;
      elseif (copD-name = "")
        copD-name := cop.player-name;
      end if;
    end if;
  end for;

  let copA = find-player(copA-name, world);
  dbg("copA= %=\n", copA);
  let possible-locations = generate-moves(world, copA);
  let copA-location-new = possible-locations[random(possible-locations.size)];
  dbg("copA-loc-new= %=\n", copA-location-new);
  
  let plan-copA = generate-plan(world, copA, copA-location-new);
  
  let copB = find-player(copB-name, world);
  possible-locations := generate-moves(world, copB);
  let copB-location-new = possible-locations[random(possible-locations.size)];
  
  let plan-copB = generate-plan(world, copB, copB-location-new);
  
  let copC = find-player(copC-name, world);
  possible-locations := generate-moves(world, copC);
  let copC-location-new = possible-locations[random(possible-locations.size)];
  
  let plan-copC = generate-plan(world, copC, copC-location-new);
  
  let copD = find-player(copD-name, world);
  possible-locations := generate-moves(world, copD);
  let copD-location-new = possible-locations[random(possible-locations.size)];
  
  let plan-copD = generate-plan(world, copD, copD-location-new);

  list(plan-copA, plan-copB, plan-copC, plan-copD)
end method make-plan;

drive-agent(make(<rookie-cop>), *standard-input*, *standard-output*);
