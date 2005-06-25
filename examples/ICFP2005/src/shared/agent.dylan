module: world

define abstract class <agent> (<object>)
  slot agent-location;
end class <agent>;

define open class <cop> (<agent>)
end class <cop>;

define open class <robber> (<agent>)
end class <robber>;

define open generic choose-move(agent :: <agent>, world :: <world>) => (move);

define open generic pick-initial-transport(cop :: <cop>) => (transport);

define open generic make-informs(cop :: <cop>, world :: <world>) => (informs);

define open generic make-plan(cop :: <cop>, world :: <world>) => (plan);

define open generic make-vote(cop :: <cop>, world :: <world>) => (vote);

define open generic drive-agent(agent :: <agent>,
                                input-stream :: <stream>,
                                output-stream :: <stream>);

define method drive-agent(agent :: <robber>,
                          input-stream :: <stream>,
                          output-stream :: <stream>)
  format(output-stream, "reg: foobar robber\n");
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
