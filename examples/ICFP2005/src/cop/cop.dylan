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
define variable copA-type :: <cop-type> = cop-car;
define variable copB-name :: <string> = "";
define variable copB-type :: <cop-type> = cop-car;
define variable copC-name :: <string> = "";
define variable copC-type :: <cop-type> = cop-car;
define variable copD-name :: <string> = "";
define variable copD-type :: <cop-type> = cop-car;

define function main(name, arguments)
  send("reg: %s %s\n", my-cop-name, my-cop-type);
  let skelet = read-world-skeleton(*standard-input*);
  my-cop-name := skelet.my-name;

  // Read world first time here.
  let our-world = read-world(*standard-input*, skelet);

  // Learn other cop's names.
  for (cop in our-world.world-players)
    if (cop.player-name ~= my-cop-name)
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
  
  block()
    while (#t)
      dbg("DEBUG: Entering cop brain.\n");
      dbg("DEBUG: Cops: %s %s %s %s %s.\n", copA-name, copB-name, copC-name, copD-name, my-cop-name);

      dbg("DEBUG: Players: ");
      for (player in our-world.world-players)
        dbg("%s ", player.player-name);
      end for;
      dbg("\n");

      let my-cop-location = find-player(my-cop-name, our-world).player-location;
      let possible-locations = find-possible-locations(my-cop-location, our-world.world-skeleton.world-edges);
      let my-cop-location-new = possible-locations[random(possible-locations.size)];

      dbg("DEBUG: Providing other cops with information.\n");

      // First step is to inform other cops of what we are doing (and maybe more).
      // Not needed for McGrufs.
      send("inf\\\n");

      // Just inform of new location.
      let inf-my-cop-new = make(<inform>,
                                bot: my-cop-name,
                                location: my-cop-location-new,
                                type: my-cop-type,
                                world: our-world.world-number + 2,
                                certainty: 100);

      inf-my-cop-new.print;
      
      send("inf/\n");

      let inform-messages = read-from-message-inform(*standard-input*);

      // Tell the other cops what to do.
      send("plan\\\n");

      let copA-location = find-player(copA-name, our-world).player-location;
      possible-locations := find-possible-locations(copA-location, our-world.world-skeleton.world-edges);
      let copA-location-new = possible-locations[random(possible-locations.size)];

      let plan-copA = make(<plan>,
                           bot: copA-name,
                           location: copA-location-new,
                           type: copA-type,
                           world: our-world.world-number + 2);

      let copB-location = find-player(copB-name, our-world).player-location;
      possible-locations := find-possible-locations(copB-location, our-world.world-skeleton.world-edges);
      let copB-location-new = possible-locations[random(possible-locations.size)];

      let plan-copB = make(<plan>,
                           bot: copB-name,
                           location: copB-location-new,
                           type: copB-type,
                           world: our-world.world-number + 2);

      let copC-location = find-player(copC-name, our-world).player-location;
      possible-locations := find-possible-locations(copC-location, our-world.world-skeleton.world-edges);
      let copC-location-new = possible-locations[random(possible-locations.size)];

      let plan-copC = make(<plan>,
                           bot: copC-name,
                           location: copC-location-new,
                           type: copC-type,
                           world: our-world.world-number + 2);

      let copD-location = find-player(copD-name, our-world).player-location;
      possible-locations := find-possible-locations(copD-location, our-world.world-skeleton.world-edges);
      let copD-location-new = possible-locations[random(possible-locations.size)];

      let plan-copD = make(<plan>,
                           bot: copD-name,
                           location: copD-location-new,
                           type: copD-type,
                           world: our-world.world-number + 2);

      plan-copA.print;
      plan-copB.print;
      plan-copC.print;
      plan-copD.print;
      
      send("plan/\n");

      let plans = read-from-message-plan(*standard-input*);

      // Vote? Winner?
      // Not needed for McGrufs.
      vote(our-world);
      let winner = read-vote-tally(*standard-input*);

      dbg("About to give command.\n");

      // Do our cop move.
      send("mov: %s %s\n", my-cop-location-new, my-cop-type);

      // Read new world.
      our-world := read-world(*standard-input*, skelet);
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

define method print (inform :: <inform>)
  if (inform.plan-world < 200) 
    send("inf: %s %s %s %d %d\n", inform.plan-bot, inform.plan-location, inform.plan-type,
         inform.plan-world, inform.inform-certainty);
  end if;
end method print;

define method print (plan :: <plan>)
  if (plan.plan-world < 200) 
    send("plan: %s %s %s %d\n", plan.plan-bot, plan.plan-location, plan.plan-type, plan.plan-world);
  end if;
end method print;

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
