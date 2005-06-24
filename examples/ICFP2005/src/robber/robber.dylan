module: robber
synopsis: 
author: 
copyright: 

define method find-player (world :: <world>) => (location)
  block(return)
    for (player in world.players)
      if (player.name = world.world-skeleton.my-name)
        return(player);
      end if;
    end for;
  end block;
end;

define method find-possible-locations
    (current-location, edges) => (result)
  let result = make(<stretchy-vector>);
  for (edge in edges)
    if (edge.type = "foot")
      if (edge.start-location = current-location)
        add!(result, edge.end-location);
      elseif (edge.end-location = current-location)
        add!(result, edge.start-location);
      end if;
    end if;
  end for;
  result;
end;
    
define function main(name, arguments)
  format-out("reg: foobar robber\n");
  force-output(*standard-output*);
  let skelet = read-world-skeleton(*standard-input*);
  block()
    while (#t)
      let world = read-world(*standard-input*, skelet);
      //format(*standard-error*, "PLAYERS %=\n", world.players);
      //force-output(*standard-error*);
      let current-location = location(find-player(world));
      //format(*standard-error*, "CURLOC %=\n", current-location);
      //force-output(*standard-error*);    
      let options = find-possible-locations(current-location, world.world-skeleton.edges);
      format-out("mov: %s robber\n", options[random(options.size)]);
      force-output(*standard-output*);
    end while;
  exception (condition :: <parse-error>)
  end;
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
