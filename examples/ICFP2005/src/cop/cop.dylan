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
  let possible-locations = find-possible-locations(cop.agent-location);
  values(possible-locations[random(possible-locations.size)], "cop-foot");
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

  let copA-location = find-player(copA-name, world).player-location;
  dbg("copA-loc= %=\n", copA-location);
  let possible-locations = find-possible-locations(copA-location);
  let copA-location-new = possible-locations[random(possible-locations.size)];
  dbg("copA-loc-new= %=\n", copA-location-new);
  
  let plan-copA = make(<plan>,
                       bot: copA-name,
                       location: copA-location-new,
                       type: copA-type,
                       world: world.world-number + 1);
  
  let copB-location = find-player(copB-name, world).player-location;
  possible-locations := find-possible-locations(copB-location);
  let copB-location-new = possible-locations[random(possible-locations.size)];
  
  let plan-copB = make(<plan>,
                       bot: copB-name,
                       location: copB-location-new,
                       type: copB-type,
                       world: world.world-number + 1);
  
  let copC-location = find-player(copC-name, world).player-location;
  possible-locations := find-possible-locations(copC-location);
  let copC-location-new = possible-locations[random(possible-locations.size)];
  
  let plan-copC = make(<plan>,
                       bot: copC-name,
                       location: copC-location-new,
                       type: copC-type,
                       world: world.world-number + 1);
  
  let copD-location = find-player(copD-name, world).player-location;
  possible-locations := find-possible-locations(copD-location);
  let copD-location-new = possible-locations[random(possible-locations.size)];
  
  let plan-copD = make(<plan>,
                       bot: copD-name,
                       location: copD-location-new,
                       type: copD-type,
                       world: world.world-number + 1);
  list(plan-copA, plan-copB, plan-copC, plan-copD)
end method make-plan;

drive-agent(make(<rookie-cop>), *standard-input*, *standard-output*);
