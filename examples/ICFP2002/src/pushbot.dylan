module: client

define class <pushbot> (<robot-agent>)
  slot push-count, init-value: 0;
  slot push-count-dir, init-value: #"up";
end class;

// return a collection of robots around location
define method get-adjacent-robots(s :: <state>, loc :: <point>)
 => (robots)
  let adj-coords = vector(point(x: loc.x, y: loc.y - 1),
			  point(x: loc.x - 1, y: loc.y),
			  point(x: loc.x + 1, y: loc.y),
			  point(x: loc.x, y: loc.y + 1));
  
  let all-adj = map(curry(robot-at, s), adj-coords);
  choose(curry(\~=, #f), all-adj);
end method;

define method update-point(p :: <point>, dir)
 => (np :: <point>)
  select(dir)
    #"north"
      => point(x: p.x, y: p.y + 1);
    #"south"
      => point(x: p.x, y: p.y - 1);
    #"west"
      => point(x: p.x - 1, y: p.y);
    #"east"
      => point(x: p.y + 1, y: p.y);
  end;
end method;

// does a transitive push check
define method check-direction(s :: <state>, p :: <point>, dir)
 => (res :: <boolean>)
  if(robot-at(s, p))
    check-direction(s, update-point(p, dir), dir);
  else
    deadly?(s.board, p);
  end;
end method;

define method robot-killable(s :: <state>, r :: <robot>)
 => (dir :: one-of(#"north", #"south", #"west", #"east", #f))
  case
    check-direction(s, point(x: r.location.x, y: r.location.y + 1), #"north")
      => #"north";
    check-direction(s, point(x: r.location.x, y: r.location.y - 1), #"south")
      => #"south";
    check-direction(s, point(x: r.location.x - 1, y: r.location.y), #"west")
      => #"west";
    check-direction(s, point(x: r.location.x + 1, y: r.location.y), #"east")
      => #"east";
    otherwise
      => #f;
  end;
/*       case
	 deadly?(s.board, point(x: r.location.x, y: r.location.y + 1))
	   => #"north";
	 deadly?(s.board, point(x: r.location.x, y: r.location.y - 1))
	   => #"south";
	 deadly?(s.board, point(x: r.location.x - 1, y: r.location.y))
	   => #"west";
	 deadly?(s.board, point(x: r.location.x + 1, y: r.location.y))
	   => #"east";
	 otherwise
	   => #f;
       end; */
end method;


define method generate-next-move(me :: <pushbot>, s :: <state>)
  => (c :: <command>)
  debug("finding me\n");
  let robot = find-robot(s, me.agent-id);
  block(return)
    debug("getting adjacent robots...");
    let adj-robots = get-adjacent-robots(s, robot.location);

    if(me.push-count-dir = #"up")
      if(~empty?(adj-robots))
	debug("robot(s) present\n");
	// if robot can be killed, bit a lot more and push
	let killable-dir = choose(curry(\~=, #f), map(curry(robot-killable, s), adj-robots));
	if(~empty?(killable-dir))
	  // TODOsort the list of robots by some metric, eg who has delivered the most
	  let targ-direction = first(killable-dir);
	  debug("..trying to kill\n");
	  return(make(<move>, bid: 50 /*(robot.money / 10) + 1*/,
		      direction: targ-direction, id: robot.id));
	end;
	debug("...no-one to kill\n");
	// if enemy robot has packages, bid a bit more and push
	let rbots = sort(adj-robots, test: method(a, b)=>(c)
					       a.inventory.size < b.inventory.size
					   end);
	let targ = first(reverse(rbots));
	let d = points-to-direction(robot.location, targ.location);
	if(d)
	  me.push-count := me.push-count + 1;
	  if(me.push-count > 4)
	    me.push-count-dir := #"down";
	  end;
	  debug("...but trying to push\n");
	  return(make(<move>, bid: 20 /*(robot.money / 50) + 1*/,
		      direction: d, id: robot.id));
	end;
	debug("...no-one to push\n");
      end;
    else
      me.push-count := me.push-count - 1;
      if(me.push-count <= 0)
	me.push-count-dir := #"up";
      end;
    end;
    
    debug("back to dumbot code\n");
    debug("DB: Considering next move (loc: %=)\n", robot.location);

    // Deliver what we can:
    let drop-these = choose(method(p)
				p.dest = robot.location;
			    end,
			    robot.inventory);
    debug("DB: drop-these = %=\n", drop-these);
    
    if (~drop-these.empty?)
      return(make(<drop>, bid: 1, package-ids: map(id, drop-these), id: robot.id));
    else 
      format-out("DB: Nothing to deliver here.\n");
      force-output(*standard-output*);
    end if;
    
    // Pick ups:
//    debug("DB: All known packages: %=\n", s.packages);
    let packages-here = packages-at(s, robot.location,
				    available-only: #t);
    debug("DB: Packages here: %=\n", packages-here);
    if (packages-here ~= #f & ~packages-here.empty?)
      let take-these = make(<vector>);
      let left = robot.capacity-left;
      // Greedy algorithm to get as much as we can:
      for (pkg in sort(packages-here, 
		       test: method (a :: <package>, b :: <package>)
			       a.weight > b.weight;
			     end method))
	if (pkg.weight <= left /* robot.capacity */
	      & find-path(robot.location, pkg.location, s.board, cutoff: 50))
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
	debug("DB: Can't pick up or deliver anything from here.\n");
      end if;
    else 
      debug("DB: No packages here (or first move)\n");
    end if;
    
    // is there another robot nearby? if so move to attack it
    if(me.push-count-dir = #"up")
      block(exit)
	debug("looing for another robot nearby...\n");
	let attack-threshold = 2;
	let robo-locations = remove(map(location, s.robots), robot.location, test: \=);
	if(empty?(robo-locations))
	  exit();
	end;
	debug("doing robo-paths\n");
	let robo-paths = map(curry(rcurry(find-path, s.board, cutoff: attack-threshold),
				   robot.location),
			     robo-locations);
	debug("removing #f's\n");
	robo-paths := choose(curry(\~=, #f), robo-paths);
	debug("sorting\n");
	robo-paths := sort!(robo-paths, stable: #t, 
			    test: method (a :: <list>, b :: <list>) 
				    a.size < b.size;
				  end method);
	debug("choosing\n");
	robo-paths := choose(method(a) => (r)
			       a.size <= attack-threshold;
			     end method,
			     robo-paths);
	if(empty?(robo-paths))
	  debug("...none\n");
	  exit();
	end;
	debug("Moving towards another robot\n");
	me.push-count := me.push-count + 1;
	  if(me.push-count > 4)
	    me.push-count-dir := #"down";
	  end;
	debug("...moving towards it\n");
	return(make-move-from-paths(robo-paths, robot));
      end;
// commenting this out may fix a double count down problem
//    else
//      me.push-count := me.push-count - 1;
//      if(me.push-count <= 0)
//	me.push-count-dir := #"up";
//      end;
    end;

    maybe-mark-base-visited(me, s, robot.location);

    // Go to the next interesting place:
    let targets = concatenate(map(dest, robot.inventory),
			      map(location, s.free-packages),
			      unvisited-bases(me, s));
    
    format-out("DB: Targets: %=\n", targets);
    force-output(*standard-output*);

    let paths = map(curry(rcurry(find-path, s.board, cutoff: 50), robot.location),
		    targets);

    format-out("DB: Paths: %=\n", paths);
    force-output(*standard-output*);

    paths := choose(conjoin(curry(\~=, #f), curry(\~=, #())), paths);

    paths := sort!(paths, stable: #t, 
		  test: method (a :: <list>, b :: <list>) 
			  a.size < b.size;
                        end method);

    return(make-move-from-paths(paths, robot));
  end block;
end method;

define method make-move-from-paths(paths, robot) => (command)
  let direction
  = if (empty?(paths))
      debug("Sorry, can't find anywhere to go!\n");
      $north
    else
      let path = paths.first;
      let new-loc = path.first;
      
      case
	new-loc = point(x: robot.location.x, y: robot.location.y + 1)
	  => $north;
	new-loc = point(x: robot.location.x + 1, y: robot.location.y)
	      => $east;
	new-loc = point(x: robot.location.x, y: robot.location.y - 1)
	      => $south;
	new-loc = point(x: robot.location.x - 1, y: robot.location.y)
	  => $west;
	new-loc = point(x: robot.location.x, y: robot.location.y)
	  => error("Can't happen");
      end case;
    end if;
  make(<move>, bid: 1, direction: direction, id: robot.id);
end method;
