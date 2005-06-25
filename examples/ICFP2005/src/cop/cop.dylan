module: cop
synopsis: 
author: 
copyright: 

define function main(name, arguments)
  format-out("reg: foobar cop-foot\n");
  force-output(*standard-output*);
  let skelet = read-world-skeleton(*standard-input*);
  block()
    while (#t)
      let world = read-world(*standard-input*, skelet);
      //inform-msg
      format-out("inf\\\n");
      format-out("inf/\n");
      force-output(*standard-output*);

      let inform-messages = read-from-message-inform(*standard-input*);

      //plan message
      format-out("plan\\\n");
      format-out("plan/\n");
      force-output(*standard-output*);

      let plans = read-from-message-plan(*standard-input*);

      //vote
      vote(world);

      let winner = read-vote-tally(*standard-input*);

      let current-location = location(find-player(world));
      let options = find-possible-locations(current-location, world.world-skeleton.edges);
      format-out("mov: %s cop-foot\n", options[random(options.size)]);
      force-output(*standard-output*);
    end while;
  exception (condition :: <parse-error>)
  end;

 exit-application(0);
end function main;

define method vote(world)
  format-out("vote\\\n");
  for (player in world.players)
    if ((player.type = "cop-foot") | (player.type = "cop-car"))
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
