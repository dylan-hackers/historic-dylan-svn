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

define method packages-i-can-carry(me :: <robot>, s :: <state>)
  => (packs)
  choose-by(method(p)
		p.weight <= me.capacity-left;
	    end method,
	    s.free-packages, map(location, s.free-packages));
end method;

// eg find-path-repeatedly(s, t, b, #[20, 50, #f])
define method find-path-repeatedly(source :: <point>,
				   target :: <point>,
				   board :: <board>,
				   #key cutoffs :: <sequence>)
  => (res :: false-or(<point-list>))
  block(return)
    for(co in cutoffs)
      debug("f-p-r: looking for path with cutoff %=\n", co);
      let r = find-path(source, target, board, cutoff: co);
      if(r ~= #f | co = #f)
	return(r);
      end;
    end;
    #f;
  end;
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
  debug("check-direction: %=\n", p);
  if(robot-at(s, p))
    let up = update-point(p, dir);
    if(up = p)
      deadly?(s.board, p);
    else
      check-direction(s, up, dir);
    end;
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
end method;


define method deal-with-adjacent-robots(me :: <pushbot>, robot :: <robot>, s :: <state>)
 => (c :: false-or(<command>))
  block(return)
    debug("getting adjacent robots...");
    let adj-robots = get-adjacent-robots(s, robot.location);

    if(me.push-count-dir = #"up")
      if(~empty?(adj-robots))
	debug("%d robot(s) adjacent\n", adj-robots.size);
	// if robot can be killed, bit a lot more and push
	let killable-dir = choose(curry(\~=, #f), map(curry(robot-killable, s), adj-robots));
	debug("got killable-dir direction\n");
	if(~empty?(killable-dir))
	  // TODOsort the list of robots by some metric, eg who has delivered the most
	  let targ-direction = first(killable-dir);
	  debug("..trying to kill\n");
	  return(make(<move>, bid: /* 50 */ truncate/(robot.money, 50) + 1,
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
	  return(make(<move>, bid: /* 20 */ truncate/(robot.money, 200) + 1,
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
    #f;
  end;
end method;

// try to pick as many as you can to the nearest bases
// sort by destination and then subsort by weight
//define method try-pickup-nearest(me :: <pushbot>, robot :: <robot>, s :: <state>)
// => (c :: false-or(<command>))
//  block(return)
    
//end method;

define method try-chase-robot(me :: <pushbot>, robot :: <robot>, s :: <state>)
 => (c :: false-or(<command>))
  // is there another robot nearby? if so move to attack it
  block(return)
    if(me.push-count-dir = #"up")
      block(exit)
	debug("looking for another robot nearby...\n");
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
    end;
    #f;
  end;
end method;

define method move-nearest-useful-place(me :: <pushbot>, robot :: <robot>, s :: <state>)
  => (c :: <command>)
  block(return)
    // Go to the next interesting place:
    debug("looking for next interesting place\n");
    let targets = #[];
    let package-weight-list = sort(map(weight, s.free-packages));
    if(~empty?(package-weight-list))
      let lightest-package = first(package-weight-list);
      if(robot.capacity-left >= lightest-package)
	targets := concatenate(map(dest, robot.inventory),
			       packages-i-can-carry(robot, s),//map(location,s.free-packages),
			       unvisited-bases(me, s));
      else
	targets := concatenate(map(dest, robot.inventory));
      end;
    else
      debug("package-weight-list is empty\n");
      targets := concatenate(packages-i-can-carry(robot, s), // any package at this point
			     unvisited-bases(me, s));
    end;
    
    format-out("DB: Targets: %=\n", targets);
    force-output(*standard-output*);

    let paths = map(curry(rcurry(find-path-repeatedly, s.board, cutoffs: #[50, #f]),
			  robot.location),
		    targets);

    format-out("DB: Paths: %=\n", paths);
    force-output(*standard-output*);

    paths := choose(conjoin(curry(\~=, #f), curry(\~=, #())), paths);

    paths := sort!(paths, stable: #t, 
		  test: method (a :: <list>, b :: <list>) 
			  a.size < b.size;
                        end method);

    return(make-move-from-paths(paths, robot));
  end;
end method;

define method generate-next-move(me :: <pushbot>, s :: <state>)
  => (c :: <command>)
  debug("finding me\n");
  let robot = find-robot(s, me.agent-id);

  block(return)

    let poss-comm = deal-with-adjacent-robots(me, robot, s);
    if(poss-comm)
      return(poss-comm);
    end;

    debug("dealt with adjacent robots\n");
    debug("DB: Considering next move (loc: %=)\n", robot.location);

    poss-comm := try-to-deliver(robot);
    if(poss-comm)
      return(poss-comm);
    end;

    poss-comm := try-pickup-many(me, robot, s);
    if(poss-comm)
      return(poss-comm);
    end;

    poss-comm := try-chase-robot(me, robot, s);
    if(poss-comm)
      return(poss-comm);
    end;

    debug("maybe marking bases\n");
    maybe-mark-base-visited(me, s, robot.location);

    move-nearest-useful-place(me, robot, s);
  end block;
end method;


