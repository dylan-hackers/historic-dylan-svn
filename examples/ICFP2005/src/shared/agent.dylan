module: world

define abstract class <agent> (<object>)
  slot agent-location;
  slot wanted-name = "DyBot";
end class <agent>;

define open abstract class <cop> (<agent>)
  slot initial-transport = "foot";
end class <cop>;

define open abstract class <robber> (<agent>)
end class <robber>;

define open generic choose-move(agent :: <agent>, world :: <world>) => (move);

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
      format(output-stream, "mov: %s robber\n",
             node-name(choose-move(agent, world)));
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

      let (target, transport) = choose-move(agent, world);
      format(output-stream, "mov: %s %s\n", target.node-name, transport); 
      force-output(output-stream);
    end while;
  exception (condition :: <parse-error>)
  end;
end method drive-agent;

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

    
