module: client

define class <pushbot> (<robot-agent>)
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

define method points-to-direction(src :: <point>, dest :: <point>)
 => (dir :: one-of(#"north", #"south", #"west", #"east", #f))
  let xdiff = src.x - dest.x;
  let ydiff = src.y - dest.y;

  if(xdiff = -1 & ydiff = 0)
    #"east";
  elseif(xdiff = 1 & ydiff = 0)
    #"west";
  elseif(xdiff = 0 & ydiff = -1)
    #"north";
  elseif(xdiff = 0 & ydiff = 1)
    #"south";
  else
    #f;
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
  let robot = find-robot(s, me.agent-id);
  block(return)
    
    let adj-robots = get-adjacent-robots(s, robot.location);
    if(~empty?(adj-robots))

      // TODO sort the list of robots by some metric, eg who has delivered the most

      // if robot can be killed, bit a lot more and push
      let killable-dir = choose(curry(\~=, #f), map(curry(robot-killable, s), adj-robots));
      if(~empty?(killable-dir))
	// TODO short this list like above
	let targ-direction = first(killable-dir);
	return(make(<move>, bid: (robot.money / 10) + 1, direction: targ-direction));
      end;
      
      // if enemy robot has packages, bid a bit more and push
      let rbots = sort(adj-robots, test: method(a, b)=>(c)
					    a.inventory.size < b.inventory.size
					end);
      let targ = first(reverse(rbots));
      let d = points-to-direction(robot.location, targ.location);
      if(d)
	return(make(<move>, bid: (robot.money / 50) + 1, direction: d));
      end;
    end;

    // Deliver what we can:
    let drop-these = choose(at-destination?, robot.inventory);
    if (~drop-these.empty?)
      return(make(<drop>, bid: 1, package-ids: map(id, drop-these)));
    end if;
    
    // Pick ups:
    let packages-here = packages-at(s, robot.location);
    if (~packages-here.empty?)
      let take-these = make(<vector>);
      let left = robot.capacity;
      // Greedy algorithm to get as much as we can:
      for (pkg in sort(packages-here))
	if (pkg.weight <= robot.capacity
	      & find-path(robot.location, pkg.location, s.board))
	  left := left - pkg.weight;
	  take-these := add!(take-these, x);
	end if;
      end for;
      return(make(<pick>, bid: 1, package-ids: map(id, take-these)));
    end if;
    
    // Go to the next interesting place:
    let targets = concatenate(map(dest, robot.inventory),
			      map(location, s.free-packages)/*,
			      map(location, choose(unvisted?,
						   s.home-bases) )*/);

    let paths = map(curry(rcurry(find-path, s.board), robot,location),
		    targets);

    paths := sort!(paths, stable: #t, 
		  test: method (a :: <list>, b :: <list>) 
			  a.size < b.size;
                        end method);
    
    let new-loc = paths.first.first;
    let direction = $North;

    if (new-loc = point(x: new-loc.x, y: new-loc.y + 1))
      direction := #"north";
    elseif (new-loc = point(x: new-loc.x + 1, y: new-loc.y))
      direction := #"east";
    elseif (new-loc = point(x: new-loc.x, y: new-loc.y - 1))
      direction := #"south";
    elseif (new-loc = point(x: new-loc.x - 1, y: new-loc.y))
      direction := #"west";
    else
      error("Can't happen");
    end if;

    return(make(<move>, bid: 1, direction: direction));
  end block;
end method;
