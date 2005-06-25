module: cop
synopsis: 
author: 
copyright: 

define constant <cop-type> = <string>;
define constant cop-foot :: <cop-type> = "cop-foot";
define constant cop-car :: <cop-type> = "cop-car";

define variable my-cop-type :: <cop-type> = cop-foot;
define variable my-cop-name :: <string> = "DyCop";

define function main(name, arguments)
  format-out("reg: %s %s\n", my-cop-name, my-cop-type);
  force-output(*standard-output*);
  let skelet = read-world-skeleton(*standard-input*);
  block()
    while (#t)
      let our-world = read-world(*standard-input*, skelet);

      format(*standard-error*, "DEBUG: Entering cop brain.\n");
      force-output(*standard-error*);    

      let my-cop-location = location(find-player(our-world));
      let possible-locations = find-possible-locations(my-cop-location, our-world.world-skeleton.edges);
      let my-cop-location-new = possible-locations[random(possible-locations.size)];

      format(*standard-error*, "DEBUG: Providing other cops with information.\n");
      force-output(*standard-error*);    

      // First step is to inform other cops of:
      //  - where we are going to move to
      //  - if we can or can't smell robber
      format-out("inf\\\n");

      let inf-my-cop-new = make(<inform>,
                                bot: my-cop-name,
                                location: my-cop-location-new,
                                type: my-cop-type,
                                world: our-world.world + 2,
                                certainty: 100);

      print-inform(inf-my-cop-new);
      
      format-out("inf/\n");
      force-output(*standard-output*);

      let inform-messages = read-from-message-inform(*standard-input*);

      format(*standard-error*, "DEBUG: Providing other cops with DER PLAN.\n");
      force-output(*standard-error*);    

      // plan message
      format-out("plan\\\n");
      format-out("plan/\n");
      force-output(*standard-output*);

      let plans = read-from-message-plan(*standard-input*);

      // vote
      vote(our-world);

      let winner = read-vote-tally(*standard-input*);

      format(*standard-error*, "About to give command.\n");
      force-output(*standard-error*);    

      format-out("mov: %s %s\n", my-cop-location-new, my-cop-type);

      force-output(*standard-output*);
    end while;
  exception (condition :: <parse-error>)
  end;

 exit-application(0);
end function main;

define method vote(world)
  format-out("vote\\\n");
  for (player in world.players)
    if ((player.type = cop-foot) | (player.type = cop-car))
      format-out("vote: %s\n", player.name);
    end if;
  end for;
  format-out("vote/\n");
  force-output(*standard-output*);
end vote;

define class <from-message-inform> (<object>)
  slot sender, init-keyword: sender:;
  slot informs, init-keyword: informs:;
end;

define method read-from-message-inform (stream)
  let res = make(<stretchy-vector>);
  let re = curry(re, stream);
  re("from\\\\");

  block()
    while(#t)
      let from-who = re("from:", name-re);
      re("inf\\\\");
      let infos = collect(stream,
                          <inform>,
                          #(bot:, location:, type:, world:, certainty:),
                          list("inf:", name-re, name-re, ptype-re, number-re, negnumber-re));
      add!(res, make(<from-message-inform>,
                     sender: from-who,
                     informs: infos));
    end while;
  exception (condition :: <parse-error>)
  end block;
  res;
end;

define method print-inform (inform :: <inform>)
  format-out("inf: %s %s %s %d %d\n", inform.bot, inform.location, inform.type,
             inform.world, inform.certainty);
end method print-inform;

define class <from-message-plan> (<object>)
  slot sender, init-keyword: sender:;
  slot plans, init-keyword: plans:;
end;

define method read-from-message-plan (stream)
  let res = make(<stretchy-vector>);
  let re = curry(re, stream);
  re("from\\\\");

  block()
    while(#t)
      let from-who = re("from:", name-re);
      re("plan\\\\");
      let infos = collect(stream,
                          <plan>,
                          #(bot:, location:, type:, world:),
                          list("plan:", name-re, name-re, ptype-re, number-re));
      add!(res, make(<from-message-plan>,
                     sender: from-who,
                     plans: infos));
    end while;
  exception (condition :: <parse-error>)
  end block;
  res;
end;

define method read-vote-tally (stream)
  let re = curry(re, stream);
  block()
    re("winner:", name-re);
  exception (cond :: <parse-error>)
    //no winner
  end;
end method;
    
main(application-name(), application-arguments());
