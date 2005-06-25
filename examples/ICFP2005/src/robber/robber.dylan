module: robber
synopsis: 
author: 
copyright: 

    
define function main(name, arguments)
  format-out("reg: foobar robber\n");
  force-output(*standard-output*);
  let skelet = read-world-skeleton(*standard-input*);
  block()
    while (#t)
      let world = read-world(*standard-input*, skelet);
      dbg("PLAYERS %=\n", world.players);

      let current-location = location(find-player(world));
      dbg("CURLOC %=\n", current-location);

      let options =
        find-possible-locations(current-location, world.world-skeleton.edges);

      dbg("Size: %d\n", options.size);
      dbg("Size: %s\n", options[1]);
      dbg("After the call.\n");

      for (i :: <integer> from 0 to options.size - 1)
        dbg("Location %d: %s\n", i, options[i]);
      end for;

      format-out("mov: %s robber\n", options[random(options.size)]);
    end while;
  exception (condition :: <parse-error>)
  end;
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
