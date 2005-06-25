module: world

define abstract class <agent> (<object>)
  slot agent-location;
  slot wanted-name = "DyBot";
end class <agent>;

define open abstract class <cop> (<agent>)
  slot initial-transport = "cop-foot";
end class <cop>;

define open abstract class <robber> (<agent>)
end class <robber>;

define open generic choose-move(agent :: <agent>, world :: <world>);

define open generic make-informs(cop :: <cop>, world :: <world>) => (informs);

define method make-informs(cop :: <cop>, world :: <world>) => (informs);
  #()
end method make-informs;

define open generic perceive-informs(informs, cop :: <cop>, world :: <world>);

define method perceive-informs(informs, cop :: <cop>, world :: <world>);
end method perceive-informs;

define open generic make-plan(cop :: <cop>, world :: <world>) => (plan);

define method make-plan(cop :: <cop>, world :: <world>) => (informs);
  #()
end method make-plan;

define open generic perceive-plans(plans, cop :: <cop>, world :: <world>);

define method perceive-plans(plans, cop :: <cop>, world :: <world>);
end method perceive-plans;

define open generic make-vote(cop :: <cop>, world :: <world>) => (vote);

define method make-vote(cop :: <cop>, world :: <world>) => (vote);
  choose(method(x)
             (x.player-type = "cop-foot") | (x.player-type = "cop-car")
         end, world.world-players)
end method make-vote;

define open generic perceive-vote(vote, cop :: <cop>, world :: <world>);

define method perceive-vote(vote, cop :: <cop>, world :: <world>);
end method perceive-vote;

define open generic drive-agent(agent :: <agent>,
                                input-stream :: <stream>,
                                output-stream :: <stream>);

define method drive-agent(agent :: <robber>,
                          input-stream :: <stream>,
                          output-stream :: <stream>)
  format(output-stream, "reg: %s robber\n", agent.wanted-name);
  force-output(output-stream);
  let skelet = read-world-skeleton(input-stream);
  block()
    while (#t)
      let world = read-world(input-stream, skelet);
      agent.agent-location 
        := find-player(skelet.my-name, world).player-location;
      //dbg("DRIVE-AGENT: %s\n", node-name(choose-move(agent, world)));
      let move = choose-move(agent, world);
      print(move);
      force-output(output-stream);
    end while;
  exception (condition :: <parse-error>)
  end;
end method drive-agent;

define method drive-agent(agent :: <cop>,
                          input-stream :: <stream>,
                          output-stream :: <stream>)
  send("reg: %s %s\n", agent.wanted-name, agent.initial-transport);
  let skelet = read-world-skeleton(*standard-input*);

  block()
    while (#t)
      let world = read-world(*standard-input*, skelet);
      agent.agent-location 
        := find-player(skelet.my-name, world).player-location;

      send("inf\\\n");
      do(print, make-informs(agent, world));
      send("inf/\n");

      perceive-informs(read-from-message-inform(input-stream),
                       agent, world);

      send("plan\\\n");
      do(print, make-plan(agent, world));
      send("plan/\n");

      perceive-plans(read-from-message-plan(input-stream),
                     agent, world);

      send("vote\\\n");
      do(method(x) send("vote: %s\n", x.player-name) end,
         make-vote(agent, world));
      send("vote/\n");
      
      perceive-vote(read-vote-tally(input-stream), agent, world);

      print(choose-move(agent, world));
    end while;
  exception (condition :: <parse-error>)
  end;
end method drive-agent;

define method print (inform :: <inform>)
  if (inform.plan-world < 200) 
    send("inf: %s %s %s %d %d\n", inform.plan-bot,
         inform.plan-location.node-name, inform.plan-type,
         inform.plan-world, inform.inform-certainty);
  end if;
end method print;

define method print (plan :: <plan>)
  if (plan.plan-world < 200) 
    send("plan: %s %s %s %d\n", plan.plan-bot,
         plan.plan-location.node-name,
         plan.plan-type, plan.plan-world);
  end if;
end method print;

    
define class <move> (<object>)
  slot target :: <node>, init-keyword: target:;
  slot transport :: <string>, init-keyword: transport:;
end class;

define method print (move :: <move>)
  send("mov: %s %s\n",
       move.target.node-name,
       move.transport);
end method;

define method generate-moves (world :: <world>, player :: <player>)
  => (move)
  let options = make(<stretchy-vector>);
    
  if ((player.player-type = "cop-foot") |
        (player.player-location.node-tag = "hq"))
    for (tar in player.player-location.moves-by-foot)
      add!(options, make(<move>,
                         target: tar,
                         transport: "cop-foot"));
    end;
  end;
  if ((player.player-type = "cop-car") | 
            (player.player-location.node-tag = "hq"))
    for (tar in player.player-location.moves-by-car)
      add!(options, make(<move>,
                         target: tar,
                         transport: "cop-car"));
    end;
  end;
  if (player.player-type = "robber")
    for (tar in player.player-location.moves-by-foot)
      add!(options, make(<move>,
                         target: tar,
                         transport: "robber"));
    end;
  end if;

  dbg("GENMOVE: type: %s\n", player.player-location.node-tag);
  for (ele in options)
    dbg("GENMOVE: %= %=\n", ele.target.node-name, ele.transport);
  end;

  options;
end method;

define method generate-plan(world :: <world>,
                            player :: <player>,
                            move :: <move>)
  => (plan :: <plan>)
  make(<plan>,
       bot: player.player-name,
       location: move.target,
       type: move.transport,
       world: world.world-number + 1);
end method;