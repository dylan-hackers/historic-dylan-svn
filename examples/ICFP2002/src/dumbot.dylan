module: client

define class <dumbot> (<robot-agent>)
end class <dumbot>;

define method generate-next-move(me :: <dumbot>, s :: <state>)
 => (c :: <command>)
  let robot = find-robot(s, me.agent-id);
  let inventory = choose(method (p :: <package>) p.carrier & p.carrier.id = robot.id; end,
			 s.packages);
  block(return)
    format-out("DB: Considering next move (loc: %=)\n", robot.location);
    force-output(*standard-output*);

    try-to-deliver(robot, return-function: return);
    try-pickup-many(me, robot, s, return-function: return);

    maybe-mark-base-visited(me, s, robot.location);

    // Go to the next interesting place:
    let targets = concatenate(map(dest, inventory),
			       map(location, choose(curry(curry(deliverable?, robot), s),
				    s.free-packages)),
			      unvisited-bases(me, s));
    
    let (target, path) = closest-point(s, robot.location, targets);

    let direction
      = if (~target)
	  debug("Sorry, can't find anywhere to go!\n");
	  make(<drop>, bid: 1, id: robot.id, package-ids: #());
	else
	  points-to-direction(robot.location, path.first);
	end if;
    return(make(<move>, bid: 1, direction: direction, id: robot.id));
  end block;
end method generate-next-move;
