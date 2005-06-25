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
  send("reg: %s %s\n", my-cop-name, my-cop-type);
  let skelet = read-world-skeleton(*standard-input*);
  block()
    while (#t)
      let our-world = read-world(*standard-input*, skelet);

      dbg("DEBUG: Entering cop brain.\n");

      let my-cop-location = find-player(our-world).player-location;
      let possible-locations = find-possible-locations(my-cop-location, our-world.world-skeleton.world-edges);
      let my-cop-location-new = possible-locations[random(possible-locations.size)];

      dbg("DEBUG: Providing other cops with information.\n");

      // First step is to inform other cops of:
      //  - where we are going to move to
      //  - if we can or can't smell robber
      send("inf\\\n");

      let inf-my-cop-new = make(<inform>,
                                bot: my-cop-name,
                                location: my-cop-location-new,
                                type: my-cop-type,
                                world: our-world.world-number + 2,
                                certainty: 100);

      print-inform(inf-my-cop-new);
      
      send("inf/\n");

      let inform-messages = read-from-message-inform(*standard-input*);

      dbg("DEBUG: Providing other cops with DER PLAN.\n");

      // plan message
      send("plan\\\n");
      send("plan/\n");

      let plans = read-from-message-plan(*standard-input*);

      // vote
      vote(our-world);

      let winner = read-vote-tally(*standard-input*);

      dbg("About to give command.\n");

      send("mov: %s %s\n", my-cop-location-new, my-cop-type);
    end while;
  exception (condition :: <parse-error>)
  end;

 exit-application(0);
end function main;

define method vote(world)
  send("vote\\\n");
  for (player in world.world-players)
    if ((player.player-type = cop-foot) | (player.player-type = cop-car))
      send("vote: %s\n", player.player-name);
    end if;
  end for;
  send("vote/\n");
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
  send("inf: %s %s %s %d %d\n", inform.plan-bot, inform.plan-location, inform.plan-type,
             inform.plan-world, inform.inform-certainty);
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
