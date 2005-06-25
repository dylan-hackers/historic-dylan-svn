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
      format(*standard-error*, "PLAYERS %=\n", world.players);
      force-output(*standard-error*);

      let current-location = location(find-player(world));
      format(*standard-error*, "CURLOC %=\n", current-location);
      force-output(*standard-error*);    

      let options =
        find-possible-locations(current-location, world.world-skeleton.edges);

      format(*standard-error*, "Size: %d\n", options.size);
      format(*standard-error*, "Size: %s\n", options[1]);
      format(*standard-error*, "After the call.\n");
      force-output(*standard-error*);    

      for (i :: <integer> from 0 to options.size - 1)
        format(*standard-error*, "Location %d: %s\n", i, options[i]);
        force-output(*standard-error*);
      end for;

      format-out("mov: %s robber\n", options[random(options.size)]);
      force-output(*standard-output*);
    end while;
  exception (condition :: <parse-error>)
  end;
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
