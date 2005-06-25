module: robber
synopsis: 
author: 
copyright: 

define class <random-walk-robber> (<robber>)
end class <random-walk-robber>;

define method choose-move(robber :: <random-walk-robber>, world :: <world>)
 => (move);
  let player = block(return)
                 for (player in world.world-players)
                   if (player.player-name = world.world-skeleton.my-name)
                     return(player);
                   end if;
                 end for;
               end block;
                 
  let options = generate-moves(world, player);
  let move = options[random(options.size)];
  dbg("moving to %s\n", move);
  move;
end method choose-move;
    
define function main(name, arguments)
  let robber = make(<random-walk-robber>);
  drive-agent(robber, *standard-input*, *standard-output*);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
