module: client

define class <dumbot> (<robot-agent>)
end class <dumbot>;

define method deliverable?(me :: <dumbot>, s :: <state>, p :: <package>, 
			   #key robot = find-robot(s, me.agent-id), 
			        capacity = robot.capacity-left)
  p.weight <= capacity & find-path(robot.location, p.location, s.board);
end method deliverable?;

define method generate-next-move(me :: <dumbot>, s :: <state>)
 => (c :: <command>)
  let robot = find-robot(s, me.agent-id);
  let inventory = choose(method (p :: <package>) p.carrier & p.carrier.id = robot.id; end,
			 s.packages);
  block(return)
    format-out("DB: Considering next move (loc: %=)\n", robot.location);
    force-output(*standard-output*);

    // Deliver what we can:
    let drop-these = choose(at-destination?, inventory);
    format-out("DB: drop-these = %=\n", drop-these);
    force-output(*standard-output*);
    
    if (~drop-these.empty?)
      return(make(<drop>, bid: 1, package-ids: map(id, drop-these), id: robot.id));
    else 
      format-out("DB: Nothing to deliver here.\n");
      force-output(*standard-output*);
    end if;
    
    // Pick ups:
    let packages-here = packages-at(s, robot.location, 
				    available-only: #t);

    if (packages-here ~= #f & ~packages-here.empty?)
      let take-these = make(<vector>);
      let left = robot.capacity-left;
      // Greedy algorithm to get as much as we can:
      for (pkg in sort(packages-here, 
		       test: method (a :: <package>, b :: <package>)
			       a.weight > b.weight;
			     end method))
	if (deliverable?(me, s, pkg, capacity: left))
	  left := left - pkg.weight;
	  take-these := add!(take-these, pkg);
	end if;
      end for;
      if (~take-these.empty?)
	return(make(<pick>, 
		    bid: 1, 
		    package-ids: map(id, take-these),
		    id: robot.id));
      else 
	format-out("DB: Can't pick up or deliver anything from here.\n");
	force-output(*standard-output*);
      end if;
    else 
      format-out("DB: No packages here (or first move)\n");
      force-output(*standard-output*);
    end if;

    maybe-mark-base-visited(me, s, robot.location);

    // Go to the next interesting place:
    let targets = concatenate(map(dest, inventory),
			       map(location, choose(curry(curry(deliverable?, me), s),
				    s.free-packages)),
			      unvisited-bases(me, s));
    
    let (target, path) = closest-point(s, robot.location, targets);

    let direction
      = if (~target)
	  error("Sorry, can't find anywhere to go!\n");
	else
	  points-to-direction(robot.location, path.first);
	end if;
    return(make(<move>, bid: 1, direction: direction, id: robot.id));
  end block;
end method generate-next-move;
