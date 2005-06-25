module: robber
synopsis: 
author: 
copyright: 

define class <random-walk-robber> (<robber>)
end class <random-walk-robber>;

define method choose-move(robber :: <random-walk-robber>, world :: <world>)
 => (move);
  dbg("PLAYERS %=\n", world.world-players);
  dbg("CURLOC %=\n", robber.agent-location);
  let options =
    find-possible-locations(robber.agent-location,
                            world.world-skeleton.world-edges);
  dbg("options = %=\n", options);
  options[random(options.size)]
end method choose-move;
    
define function main(name, arguments)
  let robber = make(<random-walk-robber>);
  drive-agent(robber, *standard-input*, *standard-output*);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
