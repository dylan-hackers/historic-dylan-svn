module: client

define class <dumbot> (<robot-agent>)
end class <dumbot>;

define method generate-next-move(me :: <dumbot>, s :: <state>)
 => (c :: <command>)
  let robot = find-robot(s, me.agent-id);
  block(return)
    format-out("DB: Considering next move (loc: %=)\n", robot.location);
    force-output(*standard-output*);

    try-to-deliver(robot, return-function: return);
    try-pickup-many(me, robot, s, return-function: return);

    maybe-mark-base-visited(me, s, robot.location);

    // Go to the next interesting place:
    let targets = concatenate(map(dest, current-inventory(s, robot)),
			       map(location, choose(curry(curry(deliverable?, robot), s),
				    s.free-packages)),
			      unvisited-bases(me, s));

    if (member?(robot.location, targets, test: \=))
      error("Current location should never be in targets!");
    end if;
    
    let (target, path) = closest-point(s, robot.location, targets);

    let direction
      = if (~target)
	  debug("Sorry, can't find anywhere to go!\n");
	  return(make(<drop>, bid: 1, id: robot.id, package-ids: #()));
	else
	  points-to-direction(robot.location, path.first);
	end if;
    return(make(<move>, bid: 1, direction: direction, id: robot.id));
  end block;
end method generate-next-move;
